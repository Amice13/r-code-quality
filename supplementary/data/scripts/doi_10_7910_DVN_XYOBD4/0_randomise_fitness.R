
########################################################################################################
### Generating null distributions for male and female additive genetic variance for relative fitness ###
########################################################################################################

## This script does the following

# 1. Load the comprehensive data set and select the data for fitness
# 2. Process the fitness data to calculate sex and sex ratio specific relatiev fitness
# 3. Create a new data "randomised" set by randomly permuting the elements of the "Family" column
# 4. Fit model (fit_nd_1 in prc_analysis.Rmd) and store output

# Load packages

library(MCMCglmm)
library(ggplot2)
library(RhpcBLASctl)
blas_set_num_threads(1)

nreps = 1
rep_id = if(Sys.info()["nodename"]=="SCE-BIO-C06645"){NA}else{commandArgs(trailingOnly = TRUE)[1]}

# Load data

d = read.csv("Comprehensive_traits.csv", header = T)

d_fit = d[d$Trait=="Fitness_Female_Female_biased"|d$Trait=="Fitness_Female_Equal"|d$Trait=="Fitness_Female_Male_biased"|d$Trait=="Fitness_Male_Female_biased"|d$Trait=="Fitness_Male_Equal"|d$Trait=="Fitness_Male_Male_biased",]

# Add columns for sex and sex ratio

d_fit$Sex = ifelse(grepl("Fitness_Female", d_fit$Trait), "Female", "Male")
d_fit$Sex.Ratio = ifelse(grepl("Equal", d_fit$Trait), "Equal", ifelse(grepl("Female_biased", d_fit$Trait), "Female_biased", "Male_biased"))

d_fit$Treatment = paste(d_fit$Sex, d_fit$Sex.Ratio)
d_fit$RelFitness = d_fit$Measurement/ave(d_fit$Measurement, d_fit$Treatment)

d_fit$Sex.Ratio = as.factor(d_fit$Sex.Ratio)
d_fit$Family = as.character(d_fit$Family)
d_fit$Family = as.factor(d_fit$Family)
d_fit$Replicate = as.factor(d_fit$Replicate)

# Create empty vectors to store model outputs

# Additive genetic covariances for relative fitness
va_fem_m_ran = rep(NA, nreps)
va_fem_e_ran = rep(NA, nreps)
va_fem_f_ran = rep(NA, nreps)
va_male_m_ran = rep(NA, nreps)
va_male_e_ran = rep(NA, nreps)
va_male_f_ran = rep(NA, nreps)

# Additive genetic covariances for relative fitness
cov_mf_f_ran = rep(NA, nreps)
cov_mf_e_ran = rep(NA, nreps)
cov_mf_m_ran = rep(NA, nreps)

# Residual variances
vr_fem_m_ran = rep(NA, nreps)
vr_fem_e_ran = rep(NA, nreps)
vr_fem_f_ran = rep(NA, nreps)
vr_male_m_ran = rep(NA, nreps)
vr_male_e_ran = rep(NA, nreps)
vr_male_f_ran = rep(NA, nreps)

day_family_ran = rep(NA, nreps)


# The loop is just for testing on the PC
# On the cluster nreps should be 1

for (rep in 1:nreps){
  
  message(paste("Replicate", rep, "in progress..."))
  
  # Create the rnadomised data set
  
  ran_vect = sample(1:nrow(d_fit), nrow(d_fit), replace = FALSE)
  d_fit_ran = d_fit
  d_fit_ran$Family = d_fit$Family[ran_vect]
  
  # Fit model
  
  prior_nd_ran = list(R = list(V = diag(6), nu = 0.002), G = list(G1 = list(V = diag(2), nu = 2, alpha.mu = c(0,0), alpha.V = diag(2)*1000), G2 = list(V = diag(2), nu = 2, alpha.mu = c(0,0), alpha.V = diag(2)*1000), G3 = list(V = diag(2), nu = 2, alpha.mu = c(0,0), alpha.V = diag(2)*1000), G4 = list(V = 1, nu = 0.002)))
  
  fit_nd_ran = MCMCglmm(RelFitness ~ Sex + Sex.Ratio + Sex:Sex.Ratio - 1, random = ~ us(Sex:at.level(Sex.Ratio, "Equal")):Family + us(Sex:at.level(Sex.Ratio, "Male_biased")):Family + us(Sex:at.level(Sex.Ratio, "Female_biased")):Family + Replicate:Family, rcov = ~idh(Sex:Sex.Ratio):units,family = "gaussian", nitt = 100000, burnin = 25000, thin=50, data = d_fit_ran, prior = prior_nd_ran, pr=T, verbose = T)
  
  # Store data
  
  va_fem_f_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexFemale:at.level(Sex.Ratio, "Female_biased"):SexFemale:at.level(Sex.Ratio, "Female_biased").Family'])
  va_fem_e_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexFemale:at.level(Sex.Ratio, "Equal"):SexFemale:at.level(Sex.Ratio, "Equal").Family'])
  va_fem_m_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexFemale:at.level(Sex.Ratio, "Male_biased"):SexFemale:at.level(Sex.Ratio, "Male_biased").Family'])
  
  va_male_f_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexMale:at.level(Sex.Ratio, "Female_biased"):SexMale:at.level(Sex.Ratio, "Female_biased").Family'])
  va_male_e_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexMale:at.level(Sex.Ratio, "Equal"):SexMale:at.level(Sex.Ratio, "Equal").Family'])
  va_male_m_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexMale:at.level(Sex.Ratio, "Male_biased"):SexMale:at.level(Sex.Ratio, "Male_biased").Family'])
  
  cov_mf_f_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexFemale:at.level(Sex.Ratio, "Female_biased"):SexMale:at.level(Sex.Ratio, "Female_biased").Family'])
  cov_mf_e_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexFemale:at.level(Sex.Ratio, "Equal"):SexMale:at.level(Sex.Ratio, "Equal").Family'])
  cov_mf_m_ran[rep] = mean(2*fit_nd_ran$VCV[,'SexFemale:at.level(Sex.Ratio, "Male_biased"):SexMale:at.level(Sex.Ratio, "Male_biased").Family'])
  
  vr_fem_f_ran[rep] = mean(fit_nd_ran$VCV[, "SexFemale:Sex.RatioFemale_biased.units"])
  vr_fem_e_ran[rep] = mean(fit_nd_ran$VCV[, "SexFemale:Sex.RatioEqual.units"])
  vr_fem_m_ran[rep] = mean(fit_nd_ran$VCV[, "SexFemale:Sex.RatioMale_biased.units"])
  
  vr_male_f_ran[rep] = mean(fit_nd_ran$VCV[, "SexMale:Sex.RatioFemale_biased.units"])
  vr_male_e_ran[rep] = mean(fit_nd_ran$VCV[, "SexMale:Sex.RatioEqual.units"])
  vr_male_m_ran[rep] = mean(fit_nd_ran$VCV[, "SexMale:Sex.RatioMale_biased.units"])
  
  day_family_ran[rep] = mean(fit_nd_ran$VCV[,"Replicate:Family"])
  
  
}

# Save data

# Create a unique stamp for this simulation

unique_stamp = as.character(paste(Sys.info()["nodename"], Sys.time()))
unique_stamp = gsub(" ", "_", unique_stamp)
unique_stamp = gsub(":", "-", unique_stamp)

output = data.frame("va_fem_f_ran" = va_fem_f_ran, "va_fem_e_ran" = va_fem_e_ran, "va_fem_m_ran" = va_fem_m_ran, 
                    "va_male_f_ran" = va_male_f_ran, "va_male_e_ran" = va_male_e_ran, "va_male_m_ran" = va_male_m_ran, 
                    "cov_mf_f_ran" = cov_mf_f_ran, "cov_mf_e_ran" = cov_mf_e_ran, "cov_mf_m_ran" = cov_mf_m_ran, 
                    "vr_fem_f_ran" = vr_fem_f_ran, "vr_fem_e_ran" = vr_fem_e_ran, "vr_fem_m_ran" = vr_fem_m_ran, 
                    "vr_male_f_ran" = vr_male_f_ran, "vr_male_e_ran" = vr_male_e_ran, "vr_male_m_ran" = vr_male_m_ran,
                    "day_family_ran" = day_family_ran)

output$unique_stamp = unique_stamp


write.csv(output, file = paste("null_distributions/fitness_randomised_rep_", rep_id,"_", unique_stamp, ".csv", sep = ""), row.names = FALSE)
