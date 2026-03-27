

library(data.table)
library(pbapply)
library(parallel)
## Packages ----

library(tidyverse)
library(forcats)
library(broom)
library(lmtest)
library(sandwich)
library(readstata13)
library(BayesLogit)

## Load source code ----
RNGkind("L'Ecuyer-CMRG")


nCores= 8 #detectCores()

the.seed=13

set.seed(the.seed)
source("code/gibbs_sim_data.R")
mc.reset.stream()
possible.strata = c('s111', 's100', 's000'
                  #  ,'s101', 's011', 's010', 's001', 's110'
                    )
betas = c(-1, 1, .15, 0.24, 0.28, 0.83, -0.01, 0.11,
          0.41, 0.62, 2, -0.13, .5,.1,.05,.01)
names(betas) = c("(Intercept)", "x1"  ,'x2',  'x3medium','x3large',"t", "z", "t:z",
                 possible.strata[possible.strata != "s000"],
                 paste0("t:", possible.strata[possible.strata != "s000"]),
                 paste0("z:", possible.strata[possible.strata != "s000"]),
                 paste0("t:z:", possible.strata[possible.strata != "s000"]))


psis = matrix(rnorm(5*length(possible.strata),0,.5), 5, length(possible.strata))

rownames(psis) = c("(Intercept)", "x1",'x2','x3medium','x3large'  )
colnames(psis) = possible.strata

psis[, "s000"] <- 0
psis[, "s111"] <- c(-2.06, 2, 0.5, 1.35, 1.75)
psis[, "s100"] <- c(-1, 1.5, 0.17, -0.28, -1.01)


  for(adj in c('weak-betas','med-betas', 'strong-betas')){

    if(adj == c('weak-betas')){
      betas['(Intercept)']=0

    betas['x1'] = 0
    betas['x2'] = 0
    betas['x3medium'] = 0
    betas['x3large'] = 0


  } else if(adj=='med-betas'){
    betas['(Intercept)']=-1
    betas['x1'] = 0.25
    betas['x2'] = 0.25
    betas['x3medium'] = 0.25
    betas['x3large'] = 0.25

  } else if(adj=='strong-betas'){
    betas['(Intercept)']=-2

    betas['x1'] = 0.5
    betas['x2'] = 0.5
    betas['x3medium'] = 0.5
    betas['x3large'] = 0.5
  }



  ###### get simulated data
    set.seed(the.seed)
  list_of_data = lapply(X=1:100, FUN = function(i){
    sim_output <- sim.gibbs.data(nrows = 1000,  monotonicity = T, stable = T, betas=betas,psis)
    return(sim_output)
  })  
  set.seed(the.seed)
l = rbindlist(mclapply(X=1:100, FUN = function(i){
 # sim_output <- sim.gibbs.data(nrows = 1000,  monotonicity = T, stable = T, betas=betas,psis)
  sim_output <- list_of_data[[i]]
  sim_data <- sim_output[[1]]
  sim_betas <- sim_output[[2]]
  sim_psis <- sim_output[[3]]


  ## calculate conditional coefficients in this data
  sim_data2 <- sim_data
  sim_data2$strata <- relevel(as.factor(sim_data2$strata), ref = "s000")
  ml.out <- nnet::multinom(strata ~ x1 + x2 + x3, data = sim_data2)
  sim_cond_psis <- t(coef(ml.out))

  l.out <- glm(y ~ x1 + x2 + x3 + t + z + t:z + strata + t:strata, data = sim_data2,
               family = binomial())
  sim_cond_betas <- coef(l.out)
  names(sim_cond_betas) <- gsub("strata", "", names(sim_cond_betas))
  sim_cond_betas <- sim_cond_betas[names(sim_betas)]


  #######


 # the.seed=13

  set.seed(the.seed)
# save results for each chain (SIMULATED DATA)
results1 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)
results2 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)
results3 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)
results4 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)

set.seed(the.seed)
resultsA <- prepost_gibbs_nocovar(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 iter = 1000, burn = 100, thin = 2)
resultsB <- prepost_gibbs_nocovar(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 iter = 1000, burn = 100, thin = 2)
resultsC <- prepost_gibbs_nocovar(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 iter = 1000, burn = 100, thin = 2)
resultsD <- prepost_gibbs_nocovar(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 iter = 1000, burn = 100, thin = 2)

out = data.table(delta.type = c('delta.1','delta.2', 'delta.1','delta.2'),
                   gibbs.type = c('Covariates', 'Covariates', 'No Covariates', 'No Covariates'),
                   bias = c(mean(c(results1$delta.1,
                   results2$delta.1,
                   results3$delta.1,
                   results4$delta.1))-sim_data$delta.1.true[1],

                            mean(c(results1$delta.2,
                            results2$delta.2,
                            results3$delta.2,
                            results4$delta.2))-sim_data$delta.2.true[1],

                            mean(c(resultsA$delta.1,
                            resultsB$delta.1,
                            resultsC$delta.1,
                            resultsD$delta.1))-sim_data$delta.1.true[1],

                            mean(c(resultsA$delta.2,
                            resultsB$delta.2,
                            resultsC$delta.2,
                            resultsD$delta.2))-sim_data$delta.2.true[1]
                   ),

                   var = c(var(c(results1$delta.1,
                   results2$delta.1,
                   results3$delta.1,
                   results4$delta.1)),

                           var(c(results1$delta.2,
                           results2$delta.2,
                           results3$delta.2,
                           results4$delta.2)),

                           var(c(resultsA$delta.1,
                           resultsB$delta.1,
                           resultsC$delta.1,
                           resultsD$delta.1)),

                           var(c(resultsA$delta.2,
                           resultsB$delta.2,
                           resultsC$delta.2,
                           resultsD$delta.2))),

               true.delta = c(sim_data$delta.1.true[1],
               sim_data$delta.2.true[1] ,
               sim_data$delta.1.true[1],
               sim_data$delta.2.true[1]   ),

               gibbs.delta  = c(mean(c(results1$delta.1,
               results2$delta.1,
               results3$delta.1,
               results4$delta.1)),
               mean(c(results1$delta.2,
               results2$delta.2,
               results3$delta.2,
               results4$delta.2)),

               mean(c(resultsA$delta.1,
               resultsB$delta.1,
               resultsC$delta.1,
               resultsD$delta.1)),
               mean(c(resultsA$delta.2,
               resultsB$delta.2,
               resultsC$delta.2,
               resultsD$delta.2))) ,


               gibbs.upper.ci.95 = c(
               quantile(c(results1$delta.1,
               results2$delta.1,
               results3$delta.1,
               results4$delta.1), probs=.975) ,

               quantile(c(results1$delta.2,
               results2$delta.2,
               results3$delta.2,
               results4$delta.2), probs=.975) ,

               quantile(c(resultsA$delta.1,
               resultsB$delta.1,
               resultsC$delta.1,
               resultsD$delta.1), probs=.975) ,

               quantile(c(resultsA$delta.2,
               resultsB$delta.2,
               resultsC$delta.2,
               resultsD$delta.2), probs=.975)
               ),




      			gibbs.lower.ci.95 = c(
               quantile(c(results1$delta.1,
               results2$delta.1,
               results3$delta.1,
               results4$delta.1), probs=1-.975) ,

               quantile(c(results1$delta.2,
               results2$delta.2,
               results3$delta.2,
               results4$delta.2), probs=1-.975) ,

               quantile(c(resultsA$delta.1,
               resultsB$delta.1,
               resultsC$delta.1,
               resultsD$delta.1), probs=1-.975) ,

               quantile(c(resultsA$delta.2,
               resultsB$delta.2,
               resultsC$delta.2,
               resultsD$delta.2), probs=1-.975)
               )




  )[,mse:=bias^2+var]

  out[,Sim.Covariates:=paste0(adj)]
  out[,iter:=i]
  return(out)
}
,  mc.set.seed=T,mc.cores=nCores
))
 fwrite(l,paste0('output/sim-',adj,'-output.csv'))
 print(paste0('output/sim-',adj,'-output.csv'))
}


