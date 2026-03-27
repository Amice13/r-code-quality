# this script outputs the gibbs simulations for various simulations used in the main paper:

# 1. Comparison of bias and efficiency gains across informativeness of the covariates
# 2. Comparison of bias and efficiency gains across violations of monotonicity and stability assumptions
# 3. Comparison of bias and efficiency gains across outcome/moderator correlation
# 4. Comparison of bias and efficiency gains across strength of priors


library(data.table)
library(pbapply)
library(parallel)
source("code/gibbs_sim_data.R")
## Packages ----

library(tidyverse)
library(forcats)
library(broom)
library(lmtest)
library(sandwich)
library(readstata13)
library(BayesLogit)

## Load source code ----
nCores= 8 #detectCores()
RNGkind("L'Ecuyer-CMRG")

the.seed=13

set.seed(the.seed)



possible.strata = c('s111', 's100', 's000'
                    ,'s101', 's011', 's010', 's001', 's110'
                    )
betas = c(-2, 1, .15, 0.24, 0.28, 0.83, -0.01, 0.11,
          0.41, 0.62, 2, -0.13, .5,.1,.05,.01, .23, .11,.02,
          .34,.5,.11,.2,-.02,.01,-.11,.67,-.13,.83,-.9,.09,-0.11,-.03,-.44,.04,.6)
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


#for(m in c(T,F)){
 # for(s in c(T,F)){
#ptm=proc.time()
m = T
s = T
  label = paste0('monotonicity-',m,'-','stable-',s)

  set.seed(the.seed)
  list_of_data = lapply(X=1:100, FUN = function(i){
    sim_output <- sim.gibbs.data(nrows = 1000,  monotonicity = m, stable = s, betas=betas,psis=psis)
    return(sim_output)
  })  
  set.seed(the.seed)
  ###### get simulated data
l = rbindlist(mclapply(1:100, FUN =function(i){
  sim_output <- list_of_data[[i]]#sim.gibbs.data(nrows = 1000, seed = NULL, monotonicity = m, stable = s, betas=betas,psis=psis)
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



  set.seed(the.seed)
# save results for each chain (SIMULATED DATA)
  # 4 chains 
results1 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)
results2 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)
results3 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)
results4 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2)

set.seed(the.seed)
results5 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F)
results6 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F)
results7 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F)
results8 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F)

set.seed(the.seed)
results9 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, stable = F)
results10 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, stable = F)
results11 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, stable = F)
results12 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, stable = F)

set.seed(the.seed)
results13 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F, stable = F)
results14 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F, stable = F)
results15 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F, stable = F)
results16 <- prepost_gibbs(y ~ t, data = sim_data, prepost = ~ z, moderator = ~ d,
                 covariates = ~ x1 + x2 + x3, iter = 1000, burn = 200, thin = 2, monotonicity = F, stable = F)



out = data.table(delta.type = c('delta.1','delta.2','delta.1','delta.2','delta.1','delta.2','delta.1','delta.2'),
                   bias = c(mean(c(results1$delta.1,
                   results2$delta.1,
                   results3$delta.1,
                   results4$delta.1))-sim_data$delta.1.true[1],

                            mean(c(results1$delta.2,
                            results2$delta.2,
                            results3$delta.2,
                            results4$delta.2))-sim_data$delta.2.true[1],


                          mean(c(results5$delta.1,
                   results6$delta.1,
                   results7$delta.1,
                   results8$delta.1))-sim_data$delta.1.true[1],

                            mean(c(results5$delta.2,
                            results6$delta.2,
                            results7$delta.2,
                            results8$delta.2))-sim_data$delta.2.true[1],


                            mean(c(results9$delta.1,
                   results10$delta.1,
                   results11$delta.1,
                   results12$delta.1))-sim_data$delta.1.true[1],

                            mean(c(results9$delta.2,
                            results10$delta.2,
                            results11$delta.2,
                            results12$delta.2))-sim_data$delta.2.true[1]  ,

                            mean(c(results13$delta.1,
                   results14$delta.1,
                   results15$delta.1,
                   results16$delta.1))-sim_data$delta.1.true[1],

                            mean(c(results13$delta.2,
                            results14$delta.2,
                            results15$delta.2,
                            results16$delta.2))-sim_data$delta.2.true[1]




                            ),



                   var = c(var(c(results1$delta.1,
                   results2$delta.1,
                   results3$delta.1,
                   results4$delta.1)),

                           var(c(results1$delta.2,
                           results2$delta.2,
                           results3$delta.2,
                           results4$delta.2)),



                           var(c(results5$delta.1,
                   results6$delta.1,
                   results7$delta.1,
                   results8$delta.1)),

                           var(c(results5$delta.2,
                           results6$delta.2,
                           results7$delta.2,
                           results8$delta.2)),


                           var(c(results9$delta.1,
                   results10$delta.1,
                   results11$delta.1,
                   results12$delta.1)),

                           var(c(results9$delta.2,
                           results10$delta.2,
                           results11$delta.2,
                           results12$delta.2)),

                           var(c(results13$delta.1,
                   results14$delta.1,
                   results15$delta.1,
                   results16$delta.1)),

                           var(c(results13$delta.2,
                           results14$delta.2,
                           results15$delta.2,
                           results16$delta.2))


                           ),

               true.delta = c(sim_data$delta.1.true[1],
               sim_data$delta.2.true[1] ,
               sim_data$delta.1.true[1],
               sim_data$delta.2.true[1],

               sim_data$delta.1.true[1],
               sim_data$delta.2.true[1] ,
               sim_data$delta.1.true[1],
               sim_data$delta.2.true[1]

                  ),

               gibbs.delta  = c(

               mean(c(results1$delta.1,
               results2$delta.1,
               results3$delta.1,
               results4$delta.1)),

               mean(c(results1$delta.2,
               results2$delta.2,
               results3$delta.2,
               results4$delta.2)),

               mean(c(results5$delta.1,
               results6$delta.1,
               results7$delta.1,
               results8$delta.1)),

               mean(c(results5$delta.2,
               results6$delta.2,
               results7$delta.2,
               results8$delta.2)),

                mean(c(results9$delta.1,
               results10$delta.1,
               results11$delta.1,
               results12$delta.1)),

               mean(c(results9$delta.2,
               results10$delta.2,
               results11$delta.2,
               results12$delta.2)),

                mean(c(results13$delta.1,
               results14$delta.1,
               results15$delta.1,
               results16$delta.1)),

               mean(c(results13$delta.2,
               results14$delta.2,
               results15$delta.2,
               results16$delta.2))



               ) ,


               gibbs.upper.ci.95 = c(

               quantile(c(results1$delta.1,
               results2$delta.1,
               results3$delta.1,
               results4$delta.1), probs=.975) ,

               quantile(c(results1$delta.2,
               results2$delta.2,
               results3$delta.2,
               results4$delta.2), probs=.975) ,

                quantile(c(results5$delta.1,
               results6$delta.1,
               results7$delta.1,
               results8$delta.1), probs=.975) ,

               quantile(c(results5$delta.2,
               results6$delta.2,
               results7$delta.2,
               results8$delta.2), probs=.975) ,

                quantile(c(results9$delta.1,
               results10$delta.1,
               results11$delta.1,
               results12$delta.1), probs=.975) ,

               quantile(c(results9$delta.2,
               results10$delta.2,
               results11$delta.2,
               results12$delta.2), probs=.975) ,

                quantile(c(results13$delta.1,
               results14$delta.1,
               results15$delta.1,
               results16$delta.1), probs=.975) ,

               quantile(c(results13$delta.2,
               results14$delta.2,
               results15$delta.2,
               results16$delta.2), probs=.975)


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

                quantile(c(results5$delta.1,
               results6$delta.1,
               results7$delta.1,
               results8$delta.1), probs=1-.975) ,

               quantile(c(results5$delta.2,
               results6$delta.2,
               results7$delta.2,
               results8$delta.2), probs=1-.975) ,

                quantile(c(results9$delta.1,
               results10$delta.1,
               results11$delta.1,
               results12$delta.1), probs=1-.975) ,

               quantile(c(results9$delta.2,
               results10$delta.2,
               results11$delta.2,
               results12$delta.2), probs=1-.975) ,

                quantile(c(results13$delta.1,
               results14$delta.1,
               results15$delta.1,
               results16$delta.1), probs=1-.975) ,

               quantile(c(results13$delta.2,
               results14$delta.2,
               results15$delta.2,
               results16$delta.2), probs=1-.975)
               )




  )[,mse:=bias^2+var]

  out[,Monotonicity:=m]
  out[,Stable:=s]
  out[,iter:=i]
  return(out)
  }
,mc.cores=nCores,
mc.set.seed = T

))
 fwrite(l,paste0('output/sim-',label,'-output.csv'))
 print(paste0('output/sim-',label,'-output.csv'))
 #proc.time()-ptm
#}

#}
