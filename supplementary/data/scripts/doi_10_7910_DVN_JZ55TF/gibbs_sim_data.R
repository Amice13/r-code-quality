# rm(list=ls()) # don't erase because we need the functions previously loaded in a separate file!
library(tidyverse)
source('code/gibbs_functions.R')


# hardcode values to walk through code inside function:
# offset=0
# monotonicity=T
# stable=T
# nrows = 1000
# seed = 02143

# function to create the simulated dataset
sim.gibbs.data = function(nrows = 1000,  monotonicity, stable, betas, psis, moderator.measurement='prepost'){

  # save possible strata
  possible.strata = c('s111', 's110', 's100','s101', 's011', 's010', 's001', 's000')
  mono.strata = c('s111', 's110', 's100', 's010', 's000')
  stable.strata = c('s111',  's100','s011', 's000')
  if(monotonicity==T){  possible.strata = possible.strata[possible.strata %in% mono.strata]}
  if(stable==T){  possible.strata = possible.strata[possible.strata %in% stable.strata]}


 # set.seed(seed)

  # create covariates
  data = data.frame( x1 = rbinom(nrows, 1, prob=0.5), # binary
                     x2 = rnorm(nrows, 1, 1),         # continuous
                     x3 = gl(3, nrows/3, length=nrows, labels=c("small", "medium", "large"))) # factor

  # save as model matrix
  covars = model.matrix(~., data)

  ## Set the "true" coefficient values
  # 1 for each covariate (with factors expanded into dummies)
  # 2 for each stratum, excluding one as the reference category, for the dummy and the interaction with t
  # +3 for coefficients on t, z, and t:z
  betas.length <- length(betas)

  # set initial values
  ## betas <- rnorm(betas.length, mean = 0, sd = 0.5)
 # betas <- c(-2, 1, 0.5, 0.94, 0.28, 0.83, -0.01, 0.11,
  #           0.41, 0.62, 2, -0.13)
  # label the columns
  # names(betas) <- c(colnames(covars), "t", "z", "t:z",
  #                   possible.strata[possible.strata != "s000"],
  #                   paste0("t:", possible.strata[possible.strata != "s000"]))
  #

  betas = betas[c(colnames(covars), "t", "z", "t:z",
                                     possible.strata[possible.strata != "s000"],
                                     paste0("t:", possible.strata[possible.strata != "s000"]),
                   paste0("z:", possible.strata[possible.strata != "s000"]),
                   paste0("t:z:", possible.strata[possible.strata != "s000"]))]
  ## create empty matrix to store the multinom coefs
  #psis.length <- length(possible.strata)
 # psis <- matrix(NA, dim(covars)[2], psis.length)
  psis = psis[colnames(covars),possible.strata]

  #colnames(psis) <- possible.strata
 # rownames(psis) <- colnames(covars)
  ## set true values
  # psis[, "s111"] <- rnorm(dim(covars)[2], 0.2, 1)
  # psis[, "s100"] <- rnorm(dim(covars)[2], 0.5, 1)
  # psis[, "s000"] <- 0
  # psis[, "s111"] <- c(-2.06, 2, 0.5, 1.35, 1.75)
  # psis[, "s100"] <- c(-1, 1.67, 0.17, -0.28, -1.01)

  ## MB: this function now gives a matrix
  omega <- update.omega(covars, psis)
  ## impute strata based on proportions
  data$strata <- apply(exp(omega),
                       MARGIN = 1,
                       FUN = function(p){
                         sample(x = possible.strata,
                                size = 1,
                                prob = p)
                       }
  )

  data$s111 <- as.numeric(data$strata == 's111')
  data$s100 <- as.numeric(data$strata == 's100')
  ## update dstar, d1, d0
  data$dstar <- ifelse(data$strata %in% c('s111', 's011', 's001', 's101'), 1, 0)
  data$d1 <- ifelse(data$strata %in% c('s111', 's110', 's101', 's100'), 1, 0)
  data$d0 <- ifelse(data$strata %in% c('s111', 's011', 's110', 's010'), 1, 0)

  ## randomly assign t=treatment; z=post-measurement
  data$t <- rbinom(nrows, 1, prob = 0.5)

  if(moderator.measurement=='prepost'){
  data$z <- rbinom(nrows, 1, prob = 0.5)
  } else if(moderator.measurement=='post'){
    data$z=1
  } else if(moderator.measurement=='pre'){
    data$z=0
  }

  mu_11 <- get.mu(data$strata, 1, 1, covars, betas, possible.strata)
  mu_10 <- get.mu(data$strata, 1, 0, covars, betas, possible.strata)
  mu_01 <- get.mu(data$strata, 0, 1, covars, betas, possible.strata)
  mu_00 <- get.mu(data$strata, 0, 0, covars, betas, possible.strata)
  data$y11 <- rbinom(nrows, 1, mu_11)
  data$y10 <- rbinom(nrows, 1, mu_10)
  data$y01 <- rbinom(nrows, 1, mu_01)
  data$y00 <- rbinom(nrows, 1, mu_00)


  ## label potential outcomes
  data$y <- NA
  data$y[data$t == 1 & data$z == 1] <- data$y11[data$t == 1 & data$z == 1]
  data$y[data$t == 1 & data$z == 0] <- data$y10[data$t == 1 & data$z == 0]
  data$y[data$t == 0 & data$z == 1] <- data$y01[data$t == 0 & data$z == 1]
  data$y[data$t == 0 & data$z == 0] <- data$y00[data$t == 0 & data$z == 0]

  ## label d (assumes stable)
  data$d <- ifelse(data$t == 0 | data$z == 0, data$dstar, data$d1)

  ####################
  ## DELTA METHOD 1 ##
  ####################


  # compute "true" delta.1
  # same computation as in the main code
  # but here we use y11 and y01 which are the predicted outcomes based on the true coefficients
  # in the gibbs sampler we use est_y11 and est_y01, which use
  # predicted outcomes based on the current coefficients (except where
  # y11 or y01 is directly observed)
  # which strata correspond to each level of D*
  dstar.1.st <- c("s111", "s011", "s001", "s101")
  dstar.1.st <- dstar.1.st[dstar.1.st %in% possible.strata]
  dstar.0.st <- c("s110", "s010", "s000", "s100")
  dstar.0.st <- dstar.0.st[dstar.0.st %in% possible.strata]
  mu.11.dstar1 <- mean(data$y11[data$strata %in% dstar.1.st])
  mu.01.dstar1 <- mean(data$y01[data$strata %in% dstar.1.st])
  mu.11.dstar0 <- mean(data$y11[data$strata %in% dstar.0.st])
  mu.01.dstar0 <- mean(data$y01[data$strata %in% dstar.0.st])


  # calculate delta
  data$delta.1.true <- (mu.11.dstar1 - mu.01.dstar1) -
    (mu.11.dstar0 - mu.01.dstar0)

  ####################
  ## DELTA METHOD 2 ##
  ####################
  ## first get predicted mu's from the logit
  mu.11 <- vector("list", length = length(possible.strata))
  mu.01 <- vector("list", length = length(possible.strata))
  names(mu.11) <- possible.strata
  names(mu.01) <- possible.strata
  for (s in possible.strata) {
    mu.11[[s]] <- get.mu(s, 1, 1, covars, betas, possible.strata)
    mu.01[[s]] <- get.mu(s, 0, 1, covars, betas, possible.strata)
  }


  ## get omegas
  # current psis = multinomial coefficients
  # omegas = log predicted strata proportions
  # convert elements of the list of psis into vectors
  # currently they are 1 row matrices
  # and this is messing everything up when we do the cbind!! argh

  ## calculate prob of each strata conditional on D* = 1, 0
  dstar1.prob <- rowSums(exp(omega[dstar.1.st]))
  rel.prob.dstar1 <- exp(omega[dstar.1.st]) / dstar1.prob
  dstar0.prob <- rowSums(exp(omega[dstar.0.st]))
  rel.prob.dstar0 <- exp(omega[dstar.0.st]) / dstar0.prob

  ## Average over the mus for those strata with the probs from above
  mu.11.dstar1 <- rowSums(mu.11[dstar.1.st] * rel.prob.dstar1)
  mu.01.dstar1 <- rowSums(mu.01[dstar.1.st] * rel.prob.dstar1)
  mu.11.dstar0 <- rowSums(mu.11[dstar.0.st] * rel.prob.dstar0)
  mu.01.dstar0 <- rowSums(mu.01[dstar.0.st] * rel.prob.dstar0)

  deltas <- (mu.11.dstar1 - mu.01.dstar1) - (mu.11.dstar0 - mu.01.dstar0)

  ## prob.s100 <- exp(omega$s100)
  ## prob.s000 <- exp(omega$s000)
  ## rel.prob.s100 <- ifelse(prob.s100==0, 0, prob.s100 / (prob.s100 + prob.s000))
  ## rel.prob.s000 <- ifelse(prob.s000==0, 0, prob.s000 / (prob.s100 + prob.s000))

  ## ## calculate delta
  ## deltas <- mu.11[["s111"]] -
  ##   mu.01[["s111"]] -
  ##   (mu.11[["s100"]] * rel.prob.s100) -
  ##   (mu.11[["s000"]] * rel.prob.s000) +
  ##   (mu.01[["s100"]] * rel.prob.s100) +
  ##   (mu.01[["s000"]] * rel.prob.s000)

  data$delta.2.true <- mean(deltas, na.rm = TRUE)
  # return the data as well as the true coefficient values so we can
  # call them in the gibbs_plot script
  return(list(data, betas, psis))
}

## run function to generate data, given true values of betas and psis
# 
# possible.strata = c('s111', 's110', 's100','s101', 's011', 's010', 's001', 's000')
# betas = rnorm(36,0,.5)
# names(betas) = c("(Intercept)", "x1"  ,        "x2"  ,        "x3medium" ,   "x3large", "t", "z", "t:z",
#                     possible.strata[possible.strata != "s000"],
#                     paste0("t:", possible.strata[possible.strata != "s000"]),
#                     paste0("z:", possible.strata[possible.strata != "s000"]),
#                     paste0("t:z:", possible.strata[possible.strata != "s000"]))
# 
# 
# psis = matrix(rnorm(5*length(possible.strata),0,.5), 5, length(possible.strata))
# 
# rownames(psis) = c("(Intercept)", "x1"  ,        "x2"  ,        "x3medium" ,   "x3large")
# colnames(psis) = possible.strata
# 
# sim_output <- sim.gibbs.data(nrows = 1000, seed = NULL, monotonicity = T, stable = T, betas=betas,psis)
# sim_data <- sim_output[[1]]
# sim_betas <- sim_output[[2]]
# sim_psis <- sim_output[[3]]
# 
# 
# ## calculate conditional coefficients in this data
# sim_data2 <- sim_data
# sim_data2$strata <- relevel(as.factor(sim_data2$strata), ref = "s000")
# ml.out <- nnet::multinom(strata ~ x1 + x2 + x3, data = sim_data2)
# sim_cond_psis <- t(coef(ml.out))
# 
# l.out <- glm(y ~ x1 + x2 + x3 + t + z + t:z + strata + t:strata, data = sim_data2,
#              family = binomial())
# sim_cond_betas <- coef(l.out)
# names(sim_cond_betas) <- gsub("strata", "", names(sim_cond_betas))
# sim_cond_betas <- sim_cond_betas[names(sim_betas)]
