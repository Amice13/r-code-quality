##
## Simulation Study: Run
##
## - load functions from `21-`, `22-`, and `23-`
## - save results to "../results/simulation"
##

## load packages
require(doParallel)
require(doMC)
require(doRNG)
require(dplyr)
require(mvnfast)
require(plm)

## load  function
source('21-simulation_setup.R')
source('22-simulation_dgp.R')
source('23-simulation_function.R')

## set cores 
cores <- parallel::detectCores() - 1
registerDoParallel(cores = cores)

## simulation setup
for (id in 1:40) {
  cat("\n id = ", id, "\n")

  ## simulation set up
  n     <- sim_grid[id, 1]
  auto1 <- auto_list[sim_grid[id, 2]]
  tr    <- trends_vec[sim_grid[id, 3]]
  bias_term <- bias_term_base[sim_grid[id, 3]]

  ## draw a fixed parameter
  set.seed(1234)
  gammas <- c(-1, 1)
  eta1   <- c(-0.25, 0.25)
  eta2   <- c(-0.3, 0.3)
  tau    <- rep(tau_mean, times = n)

  ## run sim
  sim_out <- foreach(j = 1:n_sim, .combine = "rbind",
                     .packages= c("dplyr", "plm", "mvnfast")) %dorng% ({
    ##
    ## simulation --------------------
    ## 

    ## generate data
    tmp <- sim_dgp_Time(
      n_unit = n, time = time_length, bias_term = bias_term,
      ep_sigma = ep_sigma, ep_sigma2 = ep_sigma2,
      dgp = tr,
      e_auto = e_auto, auto1 = auto1, auto2 = auto2,
      gammas = gammas, eta1 = eta1, eta2 = eta2, tau = tau
    )

    ## transform the data
    pdata <- pdata.frame(tmp$pdata, index = c("id", "time"))

    ##
    ## FE2
    ##
    FE2 <- plm(Y ~ D, data = pdata, index = c("id", "time"),
              effect = "twoways",  model = 'within'
    )

    ##
    ## each moments using the last two periods
    ##
    dids <- list()
    dids[[1]] <- plm(Y ~ D,
                    data = tmp$pdata %>% filter(time %in% c(time_length,  time_length - 1)),
                    index = c("id", "time"), effect = "twoways",  model = 'within')

    pdata[paste("yd", 0, sep = '')] <- pdata$Y
    for (j in 2:(time_length-1)) {
      pdata[paste("yd", j-1, sep = '')] <- diff(pdata[,paste("yd", j-2, sep = "")])
      fm <- as.formula(paste("yd",j-1, "~ D", sep = ''))
      dids[[j]] <- plm(fm, data = subset(pdata, time == time_length | time == (time_length -1)),
                      indx = c("id", "time"),
                      effect = 'twoways', model = 'within'
      )
    }

    ##
    ## standard DID
    ##
    DiD <- plm(Y ~ D, data = tmp$pdata %>% filter(time >= (time_length - 1)),
              index = c("id", "time"),
              effect = "twoways",  model = 'within'
    )

    ##
    ## sequential DID
    ##
    pdata$Yd <- diff(pdata$Y)
    sDID <- plm(Yd ~ D, data = pdata, indx = c("id", "time"),
                effect = 'twoways', model = 'within'
    )

    ##
    ## double DID
    ##
    DDiD <- double_did_panel_long(
      dids = dids,
      id = rep(tmp$pdata %>% filter(time >= (time_length - 1)) %>% pull(id),
              length(dids)),
      time_length = 2,
      treatment_var = "D"
    )

    use_id <- ifelse(tr == "PT", 1, 2)


    err <- c(
      DiD$coef - tau_mean,
      FE2$coef - tau_mean,
      sDID$coef - tau_mean,
      DDiD[[use_id]]$ATT - tau_mean,
      DDiD[[1]]$ATT,
      DDiD[[1]]$Var,
      DDiD[[2]]$ATT,
      DDiD[[2]]$Var
    )
    attributes(err) <- NULL
    err
  }) ## end of Monte Carlo iterations

  ## colnames
  colnames(sim_out) <- c("DID", "FE2", "sDID", "DDID",
                         "DDID-PT-ATT", "DDID-PT-Var", "DDID-PTT-ATT", "DDID-PTT_Var")

  ## save results
  fn <- paste("../results/simulation/sim_", id, ".rds", sep = "")
  saveRDS(sim_out, file = fn)
} ## end of the loop over the simulation set