library(MASS)
library(MCMCglmm)
library(ggplot2)

#####################################
### Simulations to test for power ###
#####################################

nlines = 37           # Number of hemigenomes to be sampled
nrep_t = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){14}else{as.numeric(commandArgs(trailingOnly = TRUE)[1])}  # Number of replicate measurements for each trait for each hemigenome line for the trait
nrep_w = 14           # Number of replicate measurements for fitness for each hemigenome line for fitness
nsims = 1


### Create empty vector to store estimates

vA_w_est = rep(NA, nsims)
vA_t_est = rep(NA, nsims)
COV_tw_est = rep(NA, nsims)
COV_tw_est_lower = rep(NA, nsims)
COV_tw_est_upper = rep(NA, nsims)
significance = rep(NA, nsims)

for(sim in 1:nsims){
  
  ### True additive genetic (co)variances
  
  vA_w = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){0.09}else{(as.numeric(commandArgs(trailingOnly = TRUE)[2]))}           # Additive genetic variance for relative fitness
  vA_t = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){0.13}else{(as.numeric(commandArgs(trailingOnly = TRUE)[4]))}            # Additive genetic variance for the standardised trait
  COV_tw = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){seq(0, sqrt(vA_t*vA_w), length = nsims)}else{as.numeric(commandArgs(trailingOnly = TRUE)[5])}  # The true PRC
  
  G = matrix(c(vA_w/2, COV_tw[sim]/2, COV_tw[sim]/2, vA_t/2), nrow = 2, ncol = 2) # Covariance structure for the BVs of haploid genomes
  
  ### Environmental noise ###
  
  vE_w = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){0.06}else{(as.numeric(commandArgs(trailingOnly = TRUE)[3]))}           # Residual variance for relative fitness 
  vE_t = 1 - vA_t   
  
  message(paste("Simulation", sim, "of", nsims, "in progress"))
  
  ### Sampling hemigenomes
  ### Sample breeding values of genomes in the populations (column 1 = rel. fitness, column 2 = standardised trait)
  
  BV = mvrnorm(n = nlines, mu = c(0.5, 0), Sigma = G)
  
  
  ####################################
  ### Simulating experimental data ###
  ####################################
  
  ### Trait
  
  # Create empty data frame
  
  d_trait = data.frame(matrix(NA, nrow = nlines*nrep_t, ncol = 4)) 
  colnames(d_trait) = c("line", "measurable", "measurement", "BV")
  
  d_trait$measurable = "trait"
  
  # Populate the dataframe by looping over lines and reps
  
  for(line in 1:nlines){
    
    BV_focal = BV[line, 2]  # BC for trait for the focal hemogenome
    
    for(rep in 1:nrep_t){
      
      # Sample the complementary genome from the population
      BV_complement = rnorm(1, 0, sqrt(vA_t/2))
      BV_total = (BV_focal + BV_complement)     # Breeding value for trait for this experimental individual
      trait_value = BV_total + rnorm(1, 0, sqrt(vE_t)) # Add environmental noise to obtain trait value for this individual
      
      # Add data to d_trait
      
      d_trait[(nrep_t*(line - 1) + rep), 1] = line
      d_trait[(nrep_t*(line - 1) + rep), 3] = trait_value
      d_trait[(nrep_t*(line - 1) + rep), 4] = BV_total
      
    }
    
  }
  
  
  ### Fitness
  
  # Create empty data frame
  
  d_fitness = data.frame(matrix(NA, nrow = nlines*nrep_w, ncol = 4)) 
  colnames(d_fitness) = c("line", "measurable", "measurement", "BV")
  
  d_fitness$measurable = "fitness"
  
  # Populate the dataframe by looping over lines and reps
  
  for(line in 1:nlines){
    
    BV_focal = BV[line, 1]  # BC for fitness for the focal hemogenome
    
    for(rep in 1:nrep_w){
      
      # Sample the complementary genome from the population
      BV_complement = rnorm(1, 0.5, sqrt(vA_w/2))
      BV_total = (BV_focal + BV_complement)        # Breeding value for trait for this experimental individual
      fitness_value = BV_total + rnorm(1, 0, sqrt(vE_w)) # Add environmental noise to obtain trait value for this individual
      
      # Add data to d_fitness
      
      d_fitness[(nrep_w*(line - 1) + rep), 1] = line
      d_fitness[(nrep_w*(line - 1) + rep), 3] = fitness_value
      d_fitness[(nrep_w*(line - 1) + rep), 4] = BV_total
      
    }
    
  }
  
  # Combine d_trait and d_fitness
  
  d_final = rbind(d_trait, d_fitness)
  
  #################
  ### Fit model ###
  #################
  
  prior_1 = list(R = list(V = diag(2), nu = 0.002), G = list(G1 = list(V = diag(2), nu = 2, alpha.mu = c(0,0), alpha.V = diag(2)*1000)))
  
  fit_1 = MCMCglmm(measurement ~ measurable -1, random = ~us(measurable):line, rcov = ~idh(measurable):units, family = "gaussian", nitt = 100000, burnin = 25000, thin=50, data = d_final, prior = prior_1, pr=T, verbose = T)
  
  vA_w_est[sim] = mean(2*(fit_1$VCV[,"measurablefitness:measurablefitness.line"]))
  vA_t_est[sim] = mean(2*(fit_1$VCV[,"measurabletrait:measurabletrait.line"]))
  COV_tw_est[sim] = mean(2*(fit_1$VCV[,"measurablefitness:measurabletrait.line"]))
  
  COV_tw_est_lower[sim] = HPDinterval(fit_1$VCV[,"measurablefitness:measurabletrait.line"])[1]
  COV_tw_est_upper[sim] = HPDinterval(fit_1$VCV[,"measurablefitness:measurabletrait.line"])[2]
  
  if((COV_tw_est_lower[sim]<0)&(0<COV_tw_est_upper[sim])){
    significance[sim] = 0
  }else{
    significance[sim] = 1
  }
  
  if(Sys.info()["nodename"]=="SCE-BIO-C06645"){
    plot(COV_tw_est ~ COV_tw)
    abline(0,1)
  }


}

# Fit logistic model for significance

if(Sys.info()["nodename"]=="SCE-BIO-C06645"){
  fit1 = glm(significance ~ COV_tw, family = "binomial")
  plot(significance ~ COV_tw, ylab = "Power")
  lines(predict(fit1, type = "response") ~ COV_tw)
  
}


### Save data ###

output = data.frame("nlines" = rep(nlines, nsims), 
                    "nrep_t" = rep(nrep_t, nsims), 
                    "nrep_w" = rep(nrep_w, nsims),
                    "vA_t" = rep(vA_t, nsims),
                    "vA_w" = rep(vA_w, nsims),
                    "vE_w" = rep(vE_w, nsims),
                    "COV_tw" = COV_tw,
                    "vA_t_est" = vA_t_est,
                    "vA_w_est" = vA_w_est,
                    "COV_tw_est" = COV_tw_est,
                    "COV_tw_est_lower" = COV_tw_est_lower,
                    "COV_tw_est_upper" = COV_tw_est_upper,
                    "significance" = significance)

# Create a unique stamp for this simulation

unique_stamp = as.character(paste(Sys.info()["nodename"], Sys.time()))
unique_stamp = gsub(" ", "_", unique_stamp)
unique_stamp = gsub(":", "-", unique_stamp)

output$unique_stamp = unique_stamp

write.csv(output, file = paste("null_distributions/power_analysis_", unique_stamp, ".csv", sep = ""), row.names = FALSE)

