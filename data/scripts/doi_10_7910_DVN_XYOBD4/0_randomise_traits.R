######################################################################################################
### Generating null distributions for additive genetic variance for various male and female traits ###
######################################################################################################

## This script does the following

# 1. Load the comprehensive data set and select the data for fitness
# 2. Select data for the correct trait
# 3. Create a new "randomised" data set by randomly permuting the elements of the "Family" column
# 4. Fit model and store output

# Load packages

library(MCMCglmm)
library(ggplot2)
library(RhpcBLASctl)
blas_set_num_threads(1)

nreps = 1
trait = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){"P1"}else{commandArgs(trailingOnly = TRUE)[1]}
rep_id = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){trait}else{paste(trait, "_", commandArgs(trailingOnly = TRUE)[2], sep = "")}

std = TRUE # Should the trait be standardised to mean 0 and stdev 1?

# Load data

d = read.csv("Comprehensive_traits.csv", header = T)

d_trait = d[d$Trait==trait,]

if(std){d_trait$Measurement = scale(d_trait$Measurement)}

# Create empty vectors to store model outputs
# Additive genetic, residual, and day variances for the trait

va_trait = rep(NA, nreps)
vr_trait = rep(NA, nreps)
h2_trait = rep(NA, nreps)
day_family_ran = rep(NA, nreps)

# The loop is just for testing on the PC
# On the cluster nreps should be 1

for (rep in 1:nreps){
  
  message(paste("Replicate", rep, "in progress..."))
  
  # Create the rnadomised data set
  
  ran_vect = sample(1:nrow(d_trait), nrow(d_trait), replace = FALSE)
  d_trait_ran = d_trait
  d_trait_ran$Family = d_trait$Family[ran_vect]
  
  # Fit model
  
  prior_trait_ran = list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 2, alpha.mu = 0, alpha.V = 1000), G2 = list(V = 1, nu = 0.002)))
  
  fit_trait_ran = MCMCglmm(Measurement ~ 1, random = ~ Family + Replicate:Family, family = "gaussian", nitt = 500000, burnin = 50000, thin=300, data = d_trait_ran, prior = prior_trait_ran, pr=T, verbose = T)
  
  # Store data
  va_trait[rep] = mean(2*fit_trait_ran$VCV[, "Family"])
  vr_trait[rep] = mean(fit_trait_ran$VCV[, "units"])
  h2_trait[rep] = mean(2*fit_trait_ran$VCV[, "Family"]/(fit_trait_ran$VCV[, "Family"] + fit_trait_ran$VCV[, "units"]))
  day_family_ran[rep] = mean(fit_trait_ran$VCV[, "Replicate:Family"])
  
}

# Save data

# Create a unique stamp for this simulation

unique_stamp = as.character(paste(Sys.info()["nodename"], Sys.time()))
unique_stamp = gsub(" ", "_", unique_stamp)
unique_stamp = gsub(":", "-", unique_stamp)

output = data.frame("va_trait"=va_trait, "vr_trait"=vr_trait, "h2_trait"=h2_trait, "day_family_ran"=day_family_ran)
output$Trait = trait
output$unique_stamp = unique_stamp

write.csv(output, file = paste("null_distributions/trait_standardised_randomised_rep_", rep_id,"_", unique_stamp, ".csv", sep = ""), row.names = FALSE)

