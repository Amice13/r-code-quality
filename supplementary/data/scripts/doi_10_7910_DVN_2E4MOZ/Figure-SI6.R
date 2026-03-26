########################################################
## This R-file produces Figure 6 in SI 
########################################################

rm(list = ls())

install.packages(c("tidyverse", "haven", "stats4", "grDevices", "ggrepel", "fixest", "broom", "broomExtra", "doMC"))
library(tidyverse);library(haven);library(stats4);library(grDevices)
library(ggrepel);library(fixest);library(broom);library(broomExtra)
library(doMC)

##
set.seed(123)
## set working directory as root = ""

dataRaw = read_dta("10_Data_analysis_final.dta")
## Step 1: Run models to extract observed / revealed effect size
dataPanelA = dataRaw %>% filter(!is.na(win_again_ofall_noind)) %>% mutate(master_district = factor(master_district),
                                                                          prob = 1/wid)
dataPanelB = dataRaw %>% filter(!is.na(win_again_noind)) %>% mutate(master_district = factor(master_district),
                                                                    prob = 1/wid)
############################################################
## Create dataset for each model

## Panel A model (1)(3)(5)(7): simple filter, no covariates
## Panel B model (1)(3)(5)(7): simple filter, no covariates
model5PanelA = dataPanelA %>% filter(PA3A == 0)
model7PanelA = dataPanelA %>% filter(PA3A == 1)
model5PanelB = dataPanelB %>% filter(PA3A == 0)
model7PanelB = dataPanelB %>% filter(PA3A == 1)

## Panel A model (2)(4): full sample, standardize, interact
## Panel B model (2)(4): full sample, standardize, interact 

model24PanelA = dataPanelA %>% mutate(
  master_mandate_sd = scale(master_mandate),
  coun_IND_sd = scale(coun_IND),
  coun_NRM_sd = scale(coun_NRM), 
  coun_age_sd = scale(coun_age), 
  coun_asst_motor_sd = scale(coun_asst_motor), 
  coun_terms_sd = scale(coun_terms), 
  coun_speaker_sd = scale(coun_speaker), 
  log_pop_ea_sd = scale(log_pop_ea), 
  literacy_share_ea_sd = scale(literacy_share_ea), 
  elf_ea_sd = scale(elf_ea), 
  poverty_census_ea_sd = scale(poverty_census_ea), 
  agr_share_ea_sd = scale(agr_share_ea)
)

model24PanelB = dataPanelB %>% mutate(
  master_mandate_sd = scale(master_mandate),
  coun_IND_sd = scale(coun_IND),
  coun_NRM_sd = scale(coun_NRM), 
  coun_age_sd = scale(coun_age), 
  coun_asst_motor_sd = scale(coun_asst_motor), 
  coun_terms_sd = scale(coun_terms), 
  coun_speaker_sd = scale(coun_speaker), 
  log_pop_ea_sd = scale(log_pop_ea), 
  literacy_share_ea_sd = scale(literacy_share_ea), 
  elf_ea_sd = scale(elf_ea), 
  poverty_census_ea_sd = scale(poverty_census_ea), 
  agr_share_ea_sd = scale(agr_share_ea)
)

## Panel A/B model (6): low party advantage
model6PanelA = dataPanelA %>%
  filter(PA3A == 0) %>%
  mutate(
    master_mandate_sd = scale(master_mandate),
    coun_IND_sd = scale(coun_IND),
    coun_NRM_sd = scale(coun_NRM), 
    coun_age_sd = scale(coun_age), 
    coun_asst_motor_sd = scale(coun_asst_motor), 
    coun_terms_sd = scale(coun_terms), 
    coun_speaker_sd = scale(coun_speaker), 
    log_pop_ea_sd = scale(log_pop_ea), 
    literacy_share_ea_sd = scale(literacy_share_ea), 
    elf_ea_sd = scale(elf_ea), 
    poverty_census_ea_sd = scale(poverty_census_ea), 
    agr_share_ea_sd = scale(agr_share_ea)
  )
model6PanelB = dataPanelB %>%
  filter(PA3A == 0) %>%
  mutate(
    master_mandate_sd = scale(master_mandate),
    coun_IND_sd = scale(coun_IND),
    coun_NRM_sd = scale(coun_NRM), 
    coun_age_sd = scale(coun_age), 
    coun_asst_motor_sd = scale(coun_asst_motor), 
    coun_terms_sd = scale(coun_terms), 
    coun_speaker_sd = scale(coun_speaker), 
    log_pop_ea_sd = scale(log_pop_ea), 
    literacy_share_ea_sd = scale(literacy_share_ea), 
    elf_ea_sd = scale(elf_ea), 
    poverty_census_ea_sd = scale(poverty_census_ea), 
    agr_share_ea_sd = scale(agr_share_ea)
  )

## Panel A/B model (8): high party advantage
model8PanelA = dataPanelA %>% 
  filter(PA3A == 1) %>%
  mutate(
    master_mandate_sd = scale(master_mandate),
    coun_IND_sd = scale(coun_IND),
    coun_NRM_sd = scale(coun_NRM), 
    coun_age_sd = scale(coun_age), 
    coun_asst_motor_sd = scale(coun_asst_motor), 
    coun_terms_sd = scale(coun_terms), 
    coun_speaker_sd = scale(coun_speaker), 
    log_pop_ea_sd = scale(log_pop_ea), 
    literacy_share_ea_sd = scale(literacy_share_ea), 
    elf_ea_sd = scale(elf_ea), 
    poverty_census_ea_sd = scale(poverty_census_ea), 
    agr_share_ea_sd = scale(agr_share_ea)
  )
model8PanelB = dataPanelB %>% 
  filter(PA3A == 1) %>%
  mutate(
    master_mandate_sd = scale(master_mandate),
    coun_IND_sd = scale(coun_IND),
    coun_NRM_sd = scale(coun_NRM), 
    coun_age_sd = scale(coun_age), 
    coun_asst_motor_sd = scale(coun_asst_motor), 
    coun_terms_sd = scale(coun_terms), 
    coun_speaker_sd = scale(coun_speaker), 
    log_pop_ea_sd = scale(log_pop_ea), 
    literacy_share_ea_sd = scale(literacy_share_ea), 
    elf_ea_sd = scale(elf_ea), 
    poverty_census_ea_sd = scale(poverty_census_ea), 
    agr_share_ea_sd = scale(agr_share_ea)
  )

## model (1)(3)(5)(7) needs filter based on main dataset
## model (2)(4)(6)(8) needs new dataset created above

## Controls variables needs to be standardized and interacted with treatment variable
controls = c("master_mandate_sd",'coun_NRM_sd','coun_age_sd', 'coun_asst_motor_sd', 'coun_terms_sd', 'coun_speaker_sd', 
             'log_pop_ea_sd', 'literacy_share_ea_sd', 'elf_ea_sd', 'poverty_census_ea_sd', 'agr_share_ea_sd')
nocovariates = paste0(paste0(controls,collapse = " * id + "), " * id")

## Run all models
fitPanelA1 = feols(win_again_ofall_noind ~ id | master_district, data = dataPanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA2 = feols(as.formula(paste0("win_again_ofall_noind ~ id + ",nocovariates, " | master_district")), 
                   data = model24PanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA3 = feols(win_again_ofall_noind ~ id * persig14 | master_district, data = dataPanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA4 = feols(as.formula(paste0("win_again_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model24PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA5 = feols(win_again_ofall_noind ~ id * persig14 | master_district, 
                   data = model5PanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA6 = feols(as.formula(paste0("win_again_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model6PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA7 = feols(win_again_ofall_noind ~ id * persig14 | master_district, 
                   data = model7PanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA8 = feols(as.formula(paste0("win_again_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model8PanelA, 
                   cluster = ~master_district, weights = ~wid)

fitPanelB1 = feols(win_again_noind ~ id | master_district, data = dataPanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB2 = feols(as.formula(paste0("win_again_noind ~ id + ",nocovariates, " | master_district")), 
                   data = model24PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB3 = feols(win_again_noind ~ id * persig14 | master_district, data = dataPanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB4 = feols(as.formula(paste0("win_again_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model24PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB5 = feols(win_again_noind ~ id * persig14 | master_district, 
                   data = model5PanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB6 = feols(as.formula(paste0("win_again_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model6PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB7 = feols(win_again_noind ~ id * persig14 | master_district, 
                   data = model7PanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB8 = feols(as.formula(paste0("win_again_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model8PanelB, 
                   cluster = ~master_district, weights = ~wid)


## Step 3: Start simulation

#########################
###      Panel A      ###
#########################

## Separate treatment / control samples
samplePanelA = dataRaw %>% filter(!(is.na(win_again_ofall_noind))) %>%
  mutate(
    master_mandate_sd = scale(master_mandate),
    coun_IND_sd = scale(coun_IND),
    coun_NRM_sd = scale(coun_NRM), 
    coun_age_sd = scale(coun_age), 
    coun_asst_motor_sd = scale(coun_asst_motor), 
    coun_terms_sd = scale(coun_terms), 
    coun_speaker_sd = scale(coun_speaker), 
    log_pop_ea_sd = scale(log_pop_ea), 
    literacy_share_ea_sd = scale(literacy_share_ea), 
    elf_ea_sd = scale(elf_ea), 
    poverty_census_ea_sd = scale(poverty_census_ea), 
    agr_share_ea_sd = scale(agr_share_ea)) %>%
  mutate(prob_wid = 1/wid)

controlPanelA = samplePanelA %>% filter(id == 0)
LPAcontrolPanelA = samplePanelA %>% filter(id == 0,
                                           PA3A == 0)
HPAcontrolPanelA = samplePanelA %>% filter(id == 0,
                                           PA3A == 1)
LPAPanelA = samplePanelA %>% filter(PA3A == 0)
HPAPanelA = samplePanelA %>% filter(PA3A == 1)

#########################
###      Panel B      ###
#########################

## Separate treatment / control samples
samplePanelB = dataRaw %>% filter(!(is.na(win_again_noind))) %>%
  mutate(
    master_mandate_sd = scale(master_mandate),
    coun_IND_sd = scale(coun_IND),
    coun_NRM_sd = scale(coun_NRM), 
    coun_age_sd = scale(coun_age), 
    coun_asst_motor_sd = scale(coun_asst_motor), 
    coun_terms_sd = scale(coun_terms), 
    coun_speaker_sd = scale(coun_speaker), 
    log_pop_ea_sd = scale(log_pop_ea), 
    literacy_share_ea_sd = scale(literacy_share_ea), 
    elf_ea_sd = scale(elf_ea), 
    poverty_census_ea_sd = scale(poverty_census_ea), 
    agr_share_ea_sd = scale(agr_share_ea)) %>%
  mutate(prob_wid = 1/wid)

## PanelB
controlPanelB = samplePanelB %>% filter(id == 0)
LPAcontrolPanelB = samplePanelB %>% filter(id == 0,
                                           PA3A == 0)
LPAPanelB = samplePanelB %>% filter(PA3A == 0)
HPAcontrolPanelB = samplePanelB %>% filter(id == 0,
                                           PA3A == 1)
HPAPanelB = samplePanelB %>% filter(PA3A == 1)

## Hypothesize treatment effect (the coefficients in our table)
effectPanelA = c(round(tidy(fitPanelA1)[1,2][[1]],3),round(tidy(fitPanelA2)[1,2][[1]],3),round(tidy(fitPanelA3)[1,2][[1]],3),round(tidy(fitPanelA4)[1,2][[1]],3),
                 round(tidy(fitPanelA5)[1,2][[1]],3),round(tidy(fitPanelA6)[1,2][[1]],3),round(tidy(fitPanelA7)[1,2][[1]],3),round(tidy(fitPanelA8)[1,2][[1]],3))
effectPanelB = c(round(tidy(fitPanelB1)[1,2][[1]],3),round(tidy(fitPanelB2)[1,2][[1]],3),round(tidy(fitPanelB3)[1,2][[1]],3),round(tidy(fitPanelB4)[1,2][[1]],3),
                 round(tidy(fitPanelB5)[1,2][[1]],3),round(tidy(fitPanelB6)[1,2][[1]],3),round(tidy(fitPanelB7)[1,2][[1]],3),round(tidy(fitPanelB8)[1,2][[1]],3))

controls = c("master_mandate_sd",'coun_NRM_sd', 'coun_age_sd', 'coun_asst_motor_sd', 'coun_terms_sd', 'coun_speaker_sd', 
             'log_pop_ea_sd', 'literacy_share_ea_sd', 'elf_ea_sd', 'poverty_census_ea_sd', 'agr_share_ea_sd')
nocovariates = paste0(paste0(controls,collapse = " * Z.sim + "), " * Z.sim")

alpha = 0.05   ## Standard significance level
sims = 500   ## Number of simulations to conduct for each N, N=500

#########################
###  Panel A Model 4  ###
#########################

possible.ns = seq(from=100, to=1000, by=10)
powers4 = rep(NA, length(possible.ns))
simID = rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
registerDoMC(detectCores() - 1)
foreach::foreach(j = 1:length(possible.ns)) %do% {
  N = possible.ns[j]   ## Pick the jth value for N
  
  significant.experiments = rep(NA, sims)   ## Empty object to count significant experiments
  
  simID_int = rep(NA, sims)
  
  set.seed(123)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  foreach::foreach(i = 1:sims) %do% {
    try({
      ## sample from real control group data to get control potential outcome
      sampleCount = controlPanelA %>% 
        count(master_district) %>% 
        mutate(prob = n/length(controlPanelA$master_district)) %>%
        mutate(n = floor(prob * N))
      
      sampleSim = controlPanelA %>% group_by(master_district) %>% nest() %>%
        ungroup() %>% left_join(sampleCount, by = "master_district") %>% 
        mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>%
        select(-data, -prob, -n) %>%
        unnest(samp)
      
      sampleSim$Y0 = pull(sampleSim, win_again_ofall_noind)
      
      ## Get the treat potential outcome
      ## This would vary for different models
      sampleSim$Y1 = sampleSim$Y0 + effectPanelA[4]
      
      ## Do a random assignment
      sampleSim$Z.sim = randomizr::block_ra(blocks = sampleSim$master_district,
                                            block_prob = pull(sampleSim %>%
                                                                select(master_district,prob_wid) %>% 
                                                                distinct() %>% 
                                                                arrange(master_district), prob_wid))
      ## Reveal outcomes according to assignment
      sampleSim$Y.sim = sampleSim$Y1 * sampleSim$Z.sim + sampleSim$Y0 * (1 - sampleSim$Z.sim) 
      
      ## Put treatment: id, and outcome: win_again_ofall_noind
      
      ## Reveal outcomes according to assignment
      fit.sim = feols(as.formula(paste0("Y.sim ~ Z.sim * persig14 + ",nocovariates, " | master_district")), 
                      data = sampleSim, 
                      cluster = ~master_district, weights = ~wid)
      fitSum = broom::tidy(fit.sim)
      p.value <- fitSum$p.value[fitSum$term == "Z.sim"]  # Extract p-values
      significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
      simID_int[i] <- length(sampleSim$counid)
    })
  }
  
  powers4[j] = mean(significant.experiments, na.rm=TRUE)   # store average success rate (power) for each N
  simID[j] = mean(simID_int, na.rm=TRUE)
  print(paste0("Simulation for ", N, " has finished."))
}

finalA4 = data.frame(simID, powers4)
colnames(finalA4) = c("Sim_PanelA_model4","PanelA_model4")

#write.csv(finalA4, file = file.path(root,"Data/Power Analysis Simulation/panelA_model4.csv"),row.names = FALSE)

#########################
###  Panel A Model 6  ###
#########################

possible.ns = seq(from=20, to=596, by=6)
powers6 = rep(NA, length(possible.ns))
simID = rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
registerDoMC(detectCores() - 1)
foreach::foreach(j = 1:length(possible.ns)) %do% {
  N = possible.ns[j]   ## Pick the jth value for N
  
  significant.experiments = rep(NA, sims)   ## Empty object to count significant experiments
  
  simID_int = rep(NA, sims)
  
  set.seed(123)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  foreach::foreach(i = 1:sims) %do% {
    try({
      ## sample from real control group data to get control potential outcome
      sampleCount = LPAcontrolPanelA %>% 
        count(master_district) %>% 
        mutate(prob = n/length(LPAcontrolPanelA$master_district)) %>%
        mutate(n = floor(prob * N))
      
      sampleSim = LPAcontrolPanelA %>% group_by(master_district) %>% nest() %>%
        ungroup() %>% left_join(sampleCount, by = "master_district") %>% 
        mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>%
        select(-data, -prob, -n) %>%
        unnest(samp)
      
      sampleSim$Y0 = pull(sampleSim, win_again_ofall_noind)
      
      ## Get the treat potential outcome
      ## This would vary for different models
      sampleSim$Y1 = sampleSim$Y0 + effectPanelA[6]
      
      ## Do a random assignment
      sampleSim$Z.sim = randomizr::block_ra(blocks = sampleSim$master_district,
                                            block_prob = pull(sampleSim %>%
                                                                select(master_district,prob_wid) %>% 
                                                                distinct() %>% 
                                                                arrange(master_district), prob_wid))
      ## Reveal outcomes according to assignment
      sampleSim$Y.sim = sampleSim$Y1 * sampleSim$Z.sim + sampleSim$Y0 * (1 - sampleSim$Z.sim) 
      
      ## Put treatment: id, and outcome: win_again_ofall_noind
      
      ## Reveal outcomes according to assignment
      fit.sim = feols(as.formula(paste0("Y.sim ~ Z.sim * persig14 + ",nocovariates, " | master_district")), 
                      data = sampleSim, 
                      cluster = ~master_district, weights = ~wid)
      fitSum = broom::tidy(fit.sim)
      p.value <- fitSum$p.value[fitSum$term == "Z.sim"]  # Extract p-values
      significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
      simID_int[i] <- length(sampleSim$counid)
    })
  }
  
  powers6[j] = mean(significant.experiments, na.rm=TRUE)   # store average success rate (power) for each N
  simID[j] = mean(simID_int, na.rm=TRUE)
  print(paste0("Simulation for ", N, " has finished."))
}

finalA6 = data.frame(simID, powers6)
colnames(finalA6) = c("Sim_PanelA_model6","PanelA_model6")

#write.csv(finalA6, file = file.path(root,"Data/Power Analysis Simulation/panelA_model6.csv"),row.names = FALSE)


#########################
###  Panel A Model 8  ###
#########################

possible.ns = seq(from=20, to=596, by=6)
powers8 = rep(NA, length(possible.ns))
simID = rep(NA, length(possible.ns))


#### Outer loop to vary the number of subjects ####
registerDoMC(detectCores() - 1)
foreach::foreach(j = 1:length(possible.ns)) %do% {
  N = possible.ns[j]   ## Pick the jth value for N
  
  significant.experiments = rep(NA, sims)   ## Empty object to count significant experiments
  
  simID_int = rep(NA, sims)
  
  set.seed(123)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  foreach::foreach(i = 1:sims) %do% {
    try({
      ## sample from real control group data to get control potential outcome
      sampleCount = HPAcontrolPanelA %>% 
        count(master_district) %>% 
        mutate(prob = n/length(HPAcontrolPanelA$master_district)) %>%
        mutate(n = floor(prob * N))
      
      sampleSim = HPAcontrolPanelA %>% group_by(master_district) %>% nest() %>%
        ungroup() %>% left_join(sampleCount, by = "master_district") %>% 
        mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>%
        select(-data, -prob, -n) %>%
        unnest(samp)
      
      sampleSim$Y0 = pull(sampleSim, win_again_ofall_noind)
      
      ## Get the treat potential outcome
      ## This would vary for different models
      sampleSim$Y1 = sampleSim$Y0 + effectPanelA[8]
      
      ## Do a random assignment
      sampleSim$Z.sim = randomizr::block_ra(blocks = sampleSim$master_district,
                                            block_prob = pull(sampleSim %>%
                                                                select(master_district,prob_wid) %>% 
                                                                distinct() %>% 
                                                                arrange(master_district), prob_wid))
      ## Reveal outcomes according to assignment
      sampleSim$Y.sim = sampleSim$Y1 * sampleSim$Z.sim + sampleSim$Y0 * (1 - sampleSim$Z.sim) 
      
      ## Put treatment: id, and outcome: win_again_noind
      
      ## Reveal outcomes according to assignment
      fit.sim = feols(as.formula(paste0("Y.sim ~ Z.sim * persig14 + ",nocovariates, " | master_district")), 
                      data = sampleSim, 
                      cluster = ~master_district, weights = ~wid)
      fitSum = broom::tidy(fit.sim)
      p.value <- fitSum$p.value[fitSum$term == "Z.sim"]  # Extract p-values
      significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
      simID_int[i] <- length(sampleSim$counid)
    })
  }
  
  powers8[j] = mean(significant.experiments, na.rm=TRUE)   # store average success rate (power) for each N
  simID[j] = mean(simID_int, na.rm=TRUE)
  print(paste0("Simulation for ", N, " has finished."))
}

finalA8 = data.frame(simID, powers8)
colnames(finalA8) = c("Sim_PanelA_model8","PanelA_model8")

#write.csv(finalA8, file = file.path(root,"Data/Power Analysis Simulation/PanelA_model8.csv"),row.names = FALSE)

#########################
###  Panel B Model 4  ###
#########################

possible.ns = seq(from=20, to=596, by=6)
powers4 = rep(NA, length(possible.ns))
simID = rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
registerDoMC(detectCores() - 1)
foreach::foreach(j = 1:length(possible.ns)) %do% {
  N = possible.ns[j]   ## Pick the jth value for N
  
  significant.experiments = rep(NA, sims)   ## Empty object to count significant experiments
  
  simID_int = rep(NA, sims)
  
  set.seed(123)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  foreach::foreach(i = 1:sims) %do% {
    try({
      ## sample from real control group data to get control potential outcome
      sampleCount = controlPanelB %>% 
        count(master_district) %>% 
        mutate(prob = n/length(controlPanelB$master_district)) %>%
        mutate(n = floor(prob * N))
      
      sampleSim = controlPanelB %>% group_by(master_district) %>% nest() %>%
        ungroup() %>% left_join(sampleCount, by = "master_district") %>% 
        mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>%
        select(-data, -prob, -n) %>%
        unnest(samp)
      
      sampleSim$Y0 = pull(sampleSim, win_again_noind)
      
      ## Get the treat potential outcome
      ## This would vary for different models
      sampleSim$Y1 = sampleSim$Y0 + effectPanelB[4]
      
      ## Do a random assignment
      sampleSim$Z.sim = randomizr::block_ra(blocks = sampleSim$master_district,
                                            block_prob = pull(sampleSim %>%
                                                                select(master_district,prob_wid) %>% 
                                                                distinct() %>% 
                                                                arrange(master_district), prob_wid))
      ## Reveal outcomes according to assignment
      sampleSim$Y.sim = sampleSim$Y1 * sampleSim$Z.sim + sampleSim$Y0 * (1 - sampleSim$Z.sim) 
      
      ## Put treatment: id, and outcome: win_again_noind
      
      ## Reveal outcomes according to assignment
      fit.sim = feols(as.formula(paste0("Y.sim ~ Z.sim * persig14 + ",nocovariates, " | master_district")), 
                      data = sampleSim, 
                      cluster = ~master_district, weights = ~wid)
      fitSum = broom::tidy(fit.sim)
      p.value <- fitSum$p.value[fitSum$term == "Z.sim"]  # Extract p-values
      significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
      simID_int[i] <- length(sampleSim$counid)
    })
  }
  
  powers4[j] = mean(significant.experiments, na.rm=TRUE)   # store average success rate (power) for each N
  simID[j] = mean(simID_int, na.rm=TRUE)
  print(paste0("Simulation for ", N, " has finished."))
}

finalB4 = data.frame(simID, powers4)
colnames(finalB4) = c("Sim_PanelB_model4","PanelB_model4")

#write.csv(finalB4, file = file.path(root,"Data/Power Analysis Simulation/panelB_model4.csv"),row.names = FALSE)

#########################
###  Panel B Model 6  ###
#########################

possible.ns_v1 = seq(from=20, to=150, by=3) 
possible.ns_v2 = seq(from=50, to=590, by=20) 
possible.ns = c(possible.ns_v1,possible.ns_v2)
powers6 = rep(NA, length(possible.ns))
simID = rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
registerDoMC(detectCores() - 1)
foreach::foreach(j = 1:length(possible.ns)) %do% {
  N = possible.ns[j]   ## Pick the jth value for N
  
  significant.experiments = rep(NA, sims)   ## Empty object to count significant experiments
  
  simID_int = rep(NA, sims)
  
  set.seed(123)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  foreach::foreach(i = 1:sims) %do% {
    try({
      ## sample from real control group data to get control potential outcome
      sampleCount = LPAcontrolPanelB %>% 
        count(master_district) %>% 
        mutate(prob = n/length(LPAcontrolPanelB$master_district)) %>%
        mutate(n = floor(prob * N))
      
      sampleSim = LPAcontrolPanelB %>% group_by(master_district) %>% nest() %>%
        ungroup() %>% left_join(sampleCount, by = "master_district") %>% 
        mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>%
        select(-data, -prob, -n) %>%
        unnest(samp)
      
      sampleSim$Y0 = pull(sampleSim, win_again_noind)
      
      ## Get the treat potential outcome
      ## This would vary for different models
      sampleSim$Y1 = sampleSim$Y0 + effectPanelB[6]
      
      ## Do a random assignment
      sampleSim$Z.sim = randomizr::block_ra(blocks = sampleSim$master_district,
                                            block_prob = pull(sampleSim %>%
                                                                select(master_district,prob_wid) %>% 
                                                                distinct() %>% 
                                                                arrange(master_district), prob_wid))
      ## Reveal outcomes according to assignment
      sampleSim$Y.sim = sampleSim$Y1 * sampleSim$Z.sim + sampleSim$Y0 * (1 - sampleSim$Z.sim) 
      
      ## Put treatment: id, and outcome: win_again_noind
      
      ## Reveal outcomes according to assignment
      fit.sim = feols(as.formula(paste0("Y.sim ~ Z.sim * persig14 + ",nocovariates, " | master_district")), 
                      data = sampleSim, 
                      cluster = ~master_district, weights = ~wid)
      fitSum = broom::tidy(fit.sim)
      p.value <- fitSum$p.value[fitSum$term == "Z.sim"]  # Extract p-values
      significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
      simID_int[i] <- length(sampleSim$counid)
    })
  }
  
  powers6[j] = mean(significant.experiments, na.rm=TRUE)   # store average success rate (power) for each N
  simID[j] = mean(simID_int, na.rm=TRUE)
  print(paste0("Simulation for ", N, " has finished."))
}

finalB6 = data.frame(simID, powers6)
colnames(finalB6) = c("Sim_PanelB_model6","PanelB_model6")

#write.csv(finalB6, file = file.path(root,"Data/Power Analysis Simulation/panelB_model6.csv"),row.names = FALSE)


#########################
###  Panel B Model 8  ###
#########################

possible.ns = seq(from=20, to=500, by=5) 
powers8 = rep(NA, length(possible.ns))
simID = rep(NA, length(possible.ns))


#### Outer loop to vary the number of subjects ####
registerDoMC(detectCores() - 1)
foreach::foreach(j = 1:length(possible.ns)) %do% {
  N = possible.ns[j]   ## Pick the jth value for N
  
  significant.experiments = rep(NA, sims)   ## Empty object to count significant experiments
  
  simID_int = rep(NA, sims)
  
  set.seed(123)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  foreach::foreach(i = 1:sims) %do% {
    try({
      ## sample from real control group data to get control potential outcome
      sampleCount = HPAcontrolPanelB %>% 
        count(master_district) %>% 
        mutate(prob = n/length(HPAcontrolPanelB$master_district)) %>%
        mutate(n = floor(prob * N))
      
      sampleSim = HPAcontrolPanelB %>% group_by(master_district) %>% nest() %>%
        ungroup() %>% left_join(sampleCount, by = "master_district") %>% 
        mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>%
        select(-data, -prob, -n) %>%
        unnest(samp)
      
      sampleSim$Y0 = pull(sampleSim, win_again_noind)
      
      ## Get the treat potential outcome
      ## This would vary for different models
      sampleSim$Y1 = sampleSim$Y0 + effectPanelB[8]
      
      ## Do a random assignment
      sampleSim$Z.sim = randomizr::block_ra(blocks = sampleSim$master_district,
                                            block_prob = pull(sampleSim %>%
                                                                select(master_district,prob_wid) %>% 
                                                                distinct() %>% 
                                                                arrange(master_district), prob_wid))
      ## Reveal outcomes according to assignment
      sampleSim$Y.sim = sampleSim$Y1 * sampleSim$Z.sim + sampleSim$Y0 * (1 - sampleSim$Z.sim) 
      
      ## Put treatment: id, and outcome: win_again_noind
      
      ## Reveal outcomes according to assignment
      fit.sim = feols(as.formula(paste0("Y.sim ~ Z.sim * persig14 + ",nocovariates, " | master_district")), 
                      data = sampleSim, 
                      cluster = ~master_district, weights = ~wid)
      fitSum = broom::tidy(fit.sim)
      p.value <- fitSum$p.value[fitSum$term == "Z.sim"]  # Extract p-values
      significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
      simID_int[i] <- length(sampleSim$counid)
    })
  }
  
  powers8[j] = mean(significant.experiments, na.rm=TRUE)   # store average success rate (power) for each N
  simID[j] = mean(simID_int, na.rm=TRUE)
  print(paste0("Simulation for ", N, " has finished."))
}

finalB8 = data.frame(simID, powers8)
colnames(finalB8) = c("Sim_PanelB_model8","PanelB_model8")

#write.csv(finalB8, file = file.path(root,"Data/Power Analysis Simulation/panelB_model8.csv"),row.names = FALSE)

##################################################################################
## Create the figure
##################################################################################

## We start from sample size = 60, because it's too noisy with extremely small sample sizes.
## For model 4 in Panel A, we cut the sample size to be 600 in order to make the sub-figure compatible with other sub-figures

DA4 = finalA4 %>% 
  filter(Sim_PanelA_model4 <= 600) %>%
  mutate(models = "Model 4",
         panels = "Panel A",N = 354,P = 0.8) %>%
  mutate(min = abs(Sim_PanelA_model4 - N),
         minP = abs(PanelA_model4 - P))
DA6 = finalA6 %>%
  filter(Sim_PanelA_model6 >= 60) %>% 
  mutate(models = "Model 6",panels = "Panel A",N = 166,P = 0.8) %>%
  mutate(min = abs(Sim_PanelA_model6 - N),
         minP = abs(PanelA_model6 - P))
DA8 = finalA8 %>%
  filter(Sim_PanelA_model8 >= 60) %>% 
  mutate(models = "Model 8",panels = "Panel A",N = 188,P = 0.8) %>%
  mutate(min = abs(Sim_PanelA_model8 - N),
         minP = abs(PanelA_model8 - P))

DB4 = finalB4 %>%
  filter(Sim_PanelB_model4 >= 60)%>% 
  mutate(models = "Model 4",
         panels = "Panel B",
         N = 168,
         P = 0.8) %>%
  mutate(min = abs(Sim_PanelB_model4 - N),
         minP = abs(PanelB_model4 - P))
DB6 = finalB6 %>%
  filter(Sim_PanelB_model6>=60) %>% 
  mutate(models = "Model 6",
         panels = "Panel B",
         N = 92,
         P = 0.8) %>%
  mutate(min = abs(Sim_PanelB_model6 - N),
         minP = abs(PanelB_model6 - P))

DB8 = finalB8 %>%
  filter(Sim_PanelB_model8 >= 60)%>% 
  mutate(models = "Model 8",
         panels = "Panel B",
         N = 76,
         P = 0.8) %>%
  mutate(min = abs(Sim_PanelB_model8 - N),
         minP = abs(PanelB_model8 - P))


## rename
colnames(DA4)[1:2] = c("sampleSize","Power")
colnames(DA6)[1:2] = c("sampleSize","Power")
colnames(DA8)[1:2] = c("sampleSize","Power")
colnames(DB4)[1:2] = c("sampleSize","Power")
colnames(DB6)[1:2] = c("sampleSize","Power")
colnames(DB8)[1:2] = c("sampleSize","Power")

dataFig = bind_rows(DA4,DA6, DA8, DB4, DB6, DB8)

textData = data.frame(panels = c("Panel A","Panel A","Panel A",
                                 "Panel B","Panel B","Panel B"),
                      models = c("Model 4",'Model 6','Model 8',
                                 "Model 4",'Model 6','Model 8'),
                      ES = c(effectPanelA[4],effectPanelA[6],effectPanelA[8],
                             effectPanelB[4],effectPanelB[6],effectPanelB[8]),
                      SS = c(354, 166,188,
                             168, 92, 76))

plotFig = ggplot(data = dataFig) +
  geom_point(aes(x=sampleSize ,y=Power), alpha = 0.8) +
  geom_smooth(aes(x=sampleSize ,y=Power), alpha = 0.7, method = "loess", se = FALSE, lwd = 1,
              color = "grey50") +
  scale_x_continuous(breaks=seq(0,max(dataFig$sampleSize, na.rm = TRUE),60)) +
  scale_y_continuous(breaks=seq(0,0.9,0.1)) +
  geom_segment(data = dataFig %>% group_by(panels, models) %>% filter(minP == min(minP)) %>% slice(1), 
               aes(x = sampleSize, xend = sampleSize, y=-Inf, yend = 0.8), 
               colour='red3', linetype='longdash') +
  geom_segment(data = dataFig %>% group_by(panels, models) %>% filter(minP == min(minP)) %>% slice(1), 
               aes(x = -Inf, xend = sampleSize, y=Power, yend = Power), 
               colour='red3', linetype='longdash') +
  geom_segment(data = dataFig %>% group_by(panels, models) %>% filter(min == min(min)) %>% slice(1),
               aes(x = sampleSize, xend = sampleSize, y = -Inf, yend = Power),
               colour = "royalblue4") +
  geom_segment(data = dataFig %>% group_by(panels, models) %>% filter(min == min(min)) %>% slice(1),
               aes(x = -Inf, xend = sampleSize, y = Power, yend = Power),
               colour = "royalblue4") +
  geom_text(data = textData, aes(x = 570, y =0.1,label = ES), colour = "royalblue4") + 
  geom_text(data = textData, aes(x = 365, y =0.1), label = "Effect Size (", colour = "royalblue4") + 
  geom_text(data = textData, aes(x = 423, y =0.1), label = expression(hat(beta)), colour = "royalblue4") +
  geom_text(data = textData, aes(x = 435, y =0.1), label = "):", colour = "royalblue4") +
  geom_text(data = textData, aes(x = 570, y =0.18,label = SS), colour = "royalblue4") + 
  geom_text(data = textData, aes(x = 407, y =0.18), label = "Current Sample Size: ", colour = "royalblue4") +
  geom_text(data = dataFig %>% group_by(panels, models) %>% filter(minP == min(minP)) %>% slice(1), 
            aes(x = 570, y =0.28,label = sampleSize), colour = "red3") + 
  geom_text(data = dataFig %>% group_by(panels, models) %>% filter(minP == min(minP)) %>% slice(1), 
            aes(x = 422, y =0.28), label = "80% Power Sample Size: ", colour = "red3") + 
  
  geom_text(data = dataFig %>% group_by(panels, models) %>% filter(min == min(min)) %>% slice(1), 
            aes(x = 570, y =0.23,label = round(Power,2)), colour = "royalblue4") + 
  geom_text(data = dataFig %>% group_by(panels, models) %>% filter(min == min(min)) %>% slice(1), 
            aes(x = 380, y =0.23), label = "Current Power: ", colour = "royalblue4") + 
  facet_grid(panels ~ models, scales = "free") +
  theme_bw() +
  xlab('Sample Size') +
  ylab('Power')


ggsave("Figure-SI6.pdf", plotFig, width = 15, height = 8)





