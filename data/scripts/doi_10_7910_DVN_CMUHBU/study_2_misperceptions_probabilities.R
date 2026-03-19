## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 2: misperceptions, probabilities
## Produces bootstrapped posterior probabilities, saved in 
## `study_2_probabilities.rds`. 
## Note that these are different from the realized treatment assignment 
## probabilities used in the experiment, which are based on the control-
## augmented algorithm and used to generate inverse probability weights in 
## `study_2_clean.rds`. 

version

set.seed(95126)


# Set options ----
source('utils.R')

mturk_bandit <- read_rds('study_2_clean.rds')


# Posterior probabilities ----
# ** [study_2_probabilities.rds] ----
# Bootstrapping 
batch <- mturk_bandit$batch
J <- 1e4 # iterations

Zn <- length(unique(mturk_bandit$Z))
pn <- length(unique(mturk_bandit$pid))

ppmat_dem <- ppmat_rep <- ppmat_ind <- mean_dem <- mean_rep <- mean_ind <- 
  data.frame(matrix(nrow = length(unique(batch)), ncol = Zn))

for(i in unique(batch)){

  # cat(c('\n Iter: ', i, ''))
  
  # outcomes
  ymat <- mturk_bandit %>% 
    select(
      pid, # grouping variable
      Z, # treatment variable
      Y_sum
    ) %>% 
    filter(
      batch <= i
    )
  
  # initialize parameters for each arm, each iteration
  ab <- list(alpha = matrix(NA, nrow = J, ncol = Zn*pn),
             beta = matrix(NA, nrow = J, ncol = Zn*pn))
  
  for(j in 1:J){
    ymat <- ymat %>% 
      mutate(
        d = rexp(n = n()), # exponential 1 weights, assigned by ROW
        dy1 = Y_sum*d, # successes
        dy0 = (3-Y_sum)*d, # failures
      )
    
    # update posteriors based on uniform priors (1) + data, grouped by Z, pid
    ab$alpha[j,] <- ymat %>% group_by(pid, Z) %>% 
      summarise(sum = sum(dy1) + 1, .groups = 'keep') %>% 
      pull(sum) 
    
    ab$beta[j,] <- ymat %>% group_by(pid, Z) %>% 
      summarise(sum = sum(dy0) + 1, .groups = 'keep') %>% 
      pull(sum) 
    
    if(j == 1){ # save names
      colnames(ab$alpha) <- ymat %>% 
        distinct(pid, Z) %>% 
        arrange(pid, Z) %>% 
        mutate(names = paste0(substr(pid, 1, 3), '_', Z )) %>% 
        pull(names)
    }
  }
  theta <- ab$alpha/(ab$alpha + ab$beta)
  
  ppmat_dem[i,] <- rowSums(apply(theta[,1:Zn], 1, 
                                 function(x) x==max(x) ))/nrow(theta)
  ppmat_ind[i,] <- rowSums(apply(theta[,(Zn+1):(Zn*2)], 1, 
                                 function(x) x==max(x) ))/nrow(theta)
  ppmat_rep[i,] <- rowSums(apply(theta[,(Zn*2+1):(Zn*3)], 1, 
                                 function(x) x==max(x)))/nrow(theta)
  
  mean_dem[i,] <- colMeans(theta[,1:Zn])
  mean_ind[i,] <- colMeans(theta[,(Zn+1):(Zn*2)])
  mean_rep[i,] <- colMeans(theta[,(Zn*2+1):(Zn*3)])
  
}

names(ppmat_dem) <- names(mean_dem) <- colnames(theta[, 1:Zn])
names(ppmat_ind) <- names(mean_ind) <- colnames(theta[,(Zn+1):(Zn*2)])
names(ppmat_rep) <- names(mean_rep) <- colnames(theta[,(Zn*2+1):(Zn*3)])

ppmat_dem <- rbind(rep(1/Zn, Zn), ppmat_dem)
ppmat_rep <- rbind(rep(1/Zn, Zn), ppmat_rep)
ppmat_ind <- rbind(rep(1/Zn, Zn), ppmat_ind) 

pmat <- list('ppmat_dem' = ppmat_dem,
             'ppmat_rep' = ppmat_rep,
             'ppmat_ind' = ppmat_ind)

head(pmat)

write_rds(pmat, path = 'study_2_probabilities.rds')
