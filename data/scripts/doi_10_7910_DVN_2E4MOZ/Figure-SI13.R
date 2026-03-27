########################################################
## This R-file produces Figure 13 in SI
## Figures for Panel A and Panel B will be out-sheeted separately
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "lfe", "fixest", "doMC", "foreach", "haven","stats4", "texreg", "randomizr", "broom",
                   "marginaleffexts", "grDevices", "ggrepel"))
library(tidyverse);library(lfe);library(fixest);library(doMC);library(foreach);library(haven)
library(stats4);library(texreg);library(randomizr);library(broom);library(marginaleffects);
library(grDevices);library(ggrepel)

##
set.seed(123)
## set working directory as root = ""

dataRaw = read_dta("10_Data_analysis_final.dta") %>% 
  mutate(master_district = factor(master_district), prob = 1/wid)

############################################################
## Create dataset for each model

model1PanelA = dataRaw %>% filter(!(is.na(ncand16_ofall_noind)),
                                  PA3A == 0) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))

model2PanelA = dataRaw %>% filter(!(is.na(ncand16_ofall_noind)),
                                  PA3A == 1) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))

model3PanelA = dataRaw %>% filter(!(is.na(voteshare16_ofall_noind)),
                                  PA3A == 0) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))
model4PanelA = dataRaw %>% filter(!(is.na(voteshare16_ofall_noind)),
                                  PA3A == 1) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))

model5PanelA = dataRaw %>% filter(!(is.na(eff_ncand16_ofall_noind)),
                                  PA3A == 0) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))
model6PanelA = dataRaw %>% filter(!(is.na(eff_ncand16_ofall_noind)),
                                  PA3A == 1) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))


### PaneB
model1PanelB = dataRaw %>% filter(!(is.na(ncand16_noind)),
                                  PA3A == 0) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))
model2PanelB = dataRaw %>% filter(!(is.na(ncand16_noind)),
                                  PA3A == 1) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))

model3PanelB = dataRaw %>% filter(!(is.na(voteshare16_noind)),
                                  PA3A == 0) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))
model4PanelB = dataRaw %>% filter(!(is.na(voteshare16_noind)),
                                  PA3A == 1) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))

model5PanelB = dataRaw %>% filter(!(is.na(eff_ncand16_noind)),
                                  PA3A == 0) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))
model6PanelB = dataRaw %>% filter(!(is.na(eff_ncand16_noind)),
                                  PA3A == 1) %>% 
  mutate(master_mandate_sd = scale(master_mandate),
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
         agr_share_ea_sd = scale(agr_share_ea))

## Covariates
controls = c("master_mandate_sd",'coun_NRM_sd','coun_age_sd', 'coun_asst_motor_sd', 'coun_terms_sd', 'coun_speaker_sd', 
             'log_pop_ea_sd', 'literacy_share_ea_sd', 'elf_ea_sd', 'poverty_census_ea_sd', 'agr_share_ea_sd')
nocovariates = paste0(paste0(controls,collapse = " * id + "), " * id")

#### Run all models
fitPanelA1 = feols(as.formula(paste0("ncand16_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model1PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA2 = feols(as.formula(paste0("ncand16_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model2PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA3 = feols(as.formula(paste0("voteshare16_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model3PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA4 = feols(as.formula(paste0("voteshare16_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model4PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA5 = feols(as.formula(paste0("eff_ncand16_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model5PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA6 = feols(as.formula(paste0("eff_ncand16_ofall_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model6PanelA, 
                   cluster = ~master_district, weights = ~wid)

fitPanelB1 = feols(as.formula(paste0("ncand16_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model1PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB2 = feols(as.formula(paste0("ncand16_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model2PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB3 = feols(as.formula(paste0("voteshare16_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model3PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB4 = feols(as.formula(paste0("voteshare16_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model4PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB5 = feols(as.formula(paste0("eff_ncand16_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model5PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB6 = feols(as.formula(paste0("eff_ncand16_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model6PanelB, 
                   cluster = ~master_district, weights = ~wid)


## Marginal effects for high performance group
marginA1 = marginaleffects(fitPanelA1, newdata = datagrid(persig14 = model1PanelA$persig14), variables = "id")
marginA2 = marginaleffects(fitPanelA2, newdata = datagrid(persig14 = model2PanelA$persig14), variables = "id")
marginA3 = marginaleffects(fitPanelA3, newdata = datagrid(persig14 = model3PanelA$persig14), variables = "id")
marginA4 = marginaleffects(fitPanelA4, newdata = datagrid(persig14 = model4PanelA$persig14), variables = "id")
marginA5 = marginaleffects(fitPanelA5, newdata = datagrid(persig14 = model5PanelA$persig14), variables = "id")
marginA6 = marginaleffects(fitPanelA6, newdata = datagrid(persig14 = model6PanelA$persig14), variables = "id")

marginB1 = marginaleffects(fitPanelB1, newdata = datagrid(persig14 = model1PanelB$persig14), variables = "id")
marginB2 = marginaleffects(fitPanelB2, newdata = datagrid(persig14 = model2PanelB$persig14), variables = "id")
marginB3 = marginaleffects(fitPanelB3, newdata = datagrid(persig14 = model3PanelB$persig14), variables = "id")
marginB4 = marginaleffects(fitPanelB4, newdata = datagrid(persig14 = model4PanelB$persig14), variables = "id")
marginB5 = marginaleffects(fitPanelB5, newdata = datagrid(persig14 = model5PanelB$persig14), variables = "id")
marginB6 = marginaleffects(fitPanelB6, newdata = datagrid(persig14 = model6PanelB$persig14), variables = "id")

#######################################################################
## Collect and calculate RI P-vals

effectPanelA = c(tidy(fitPanelA1)[1,2][[1]],tidy(fitPanelA2)[1,2][[1]],tidy(fitPanelA3)[1,2][[1]],tidy(fitPanelA4)[1,2][[1]],
                 tidy(fitPanelA5)[1,2][[1]],tidy(fitPanelA6)[1,2][[1]])
effectPanelB = c(tidy(fitPanelB1)[1,2][[1]],tidy(fitPanelB2)[1,2][[1]],tidy(fitPanelB3)[1,2][[1]],tidy(fitPanelB4)[1,2][[1]],
                 tidy(fitPanelB5)[1,2][[1]],tidy(fitPanelB6)[1,2][[1]])

effectPanel = c(effectPanelA,effectPanelB)

## n = 1:12 represents model (1) to (8) in Panel A, then model (1) to (8) in Panel B
## in the same order as effect size vector `effectPanel`
registerDoMC(detectCores()-1)
foreach::foreach(n=1:12)%do%{
  set.seed(123)
  ## Initiate vectors to score statistics
  SimID = c()
  coefs = c()
  ses = c()
  tstats = c()
  pvals = c()
  obs = c()
  r2s = c()
  
  for(i in 1:10000){
  #for(i in 1:20){
    SimID = c(SimID, paste0("SimN",i))
    if(n==1){dataSim=model1PanelA}else if(n==2){dataSim=model2PanelA}else if(n==3){dataSim=model3PanelA}else if(n==4){
      dataSim=model4PanelA}else if(n==5){dataSim=model5PanelA}else if(n==6){dataSim=model6PanelA
      }else if(n==7){dataSim=model1PanelB}else if(n==8){dataSim=model2PanelB}else if(n==9){dataSim=model3PanelB
      }else if(n==10){dataSim=model4PanelB}else if(n==11){dataSim=model5PanelB}else if(n==12){dataSim=model6PanelB}
    
    ## random assign treatment, based on original treatment assignment probability in each district
    dataSim$treatment = block_ra(blocks = as.vector(dataSim$master_district),
                                 block_prob = pull(dataSim %>%
                                                     select(master_district,prob) %>% 
                                                     distinct() %>% 
                                                     arrange(master_district), prob))
    if(n==1){fitSim = feols(as.formula(paste0("ncand16_ofall_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                            data = dataSim, cluster = ~master_district, weights = ~wid)
    }else if(n==2){fitSim = feols(as.formula(paste0("ncand16_ofall_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, cluster = ~master_district, weights = ~wid)
    }else if(n==3){fitSim = feols(as.formula(paste0("voteshare16_ofall_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, 
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==4){fitSim = feols(as.formula(paste0("voteshare16_ofall_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, 
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==5){fitSim = feols(as.formula(paste0("eff_ncand16_ofall_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, 
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==6){fitSim = feols(as.formula(paste0("eff_ncand16_ofall_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, 
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==7){fitSim = feols(as.formula(paste0("ncand16_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, 
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==8){fitSim = feols(as.formula(paste0("ncand16_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, 
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==9){fitSim = feols(as.formula(paste0("voteshare16_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                  data = dataSim, 
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==10){fitSim = feols(as.formula(paste0("voteshare16_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                   data = dataSim, 
                                   cluster = ~master_district, weights = ~wid)
    }else if(n==11){fitSim = feols(as.formula(paste0("eff_ncand16_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                   data = dataSim, 
                                   cluster = ~master_district, weights = ~wid)
    }else if(n==12){fitSim = feols(as.formula(paste0("eff_ncand16_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                   data = dataSim, 
                                   cluster = ~master_district, weights = ~wid)}
    
    sumSim = tidy(fitSim)
    coefs = c(coefs, sumSim$estimate[sumSim$term == "treatment"])
    ses = c(ses, sumSim$std.error[sumSim$term == "treatment"])
    tstats = c(tstats, sumSim$statistic[sumSim$term == "treatment"])
    pvals = c(pvals, sumSim$p.value[sumSim$term == "treatment"])
    obs = c(obs, fitSim$nobs)
    r2s = c(r2s, glance(fitSim)$r.squared)}
  
  print(paste0("D", n," dataset has been finished."))
  simResult = data.frame(SimID, coefs, ses, tstats, pvals, obs, r2s)
  ## Calculate the randomization inference P-value
  simResult$indicator = (abs(simResult$coefs) > abs(effectPanel[n]))
  
  ## Outsheet simulation results
  assign(paste0("D",n), simResult)}


#######################################################################
## Figures of the Randomized Inference Simulations
#######################################################################


effectPanelA = c(round(tidy(fitPanelA1)[1,2][[1]],3),round(tidy(fitPanelA2)[1,2][[1]],3),round(tidy(fitPanelA3)[1,2][[1]],3),round(tidy(fitPanelA4)[1,2][[1]],3),
                 round(tidy(fitPanelA5)[1,2][[1]],3),round(tidy(fitPanelA6)[1,2][[1]],3))
effectPanelB = c(round(tidy(fitPanelB1)[1,2][[1]],3),round(tidy(fitPanelB2)[1,2][[1]],3),round(tidy(fitPanelB3)[1,2][[1]],3),round(tidy(fitPanelB4)[1,2][[1]],3),
                 round(tidy(fitPanelB5)[1,2][[1]],3),round(tidy(fitPanelB6)[1,2][[1]],3))

dataFigPanelA = bind_rows(D1 %>% mutate(models = "Model 1",
                                        panels = "Panel A",
                                        realCoef = effectPanelA[1],
                                        sigCoef = round(quantile(D1$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D1$coefs, 0.975),3),
                                        N = "114",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D1$indicator),3),"]")),
                          D2 %>% mutate(models = "Model 2",
                                        panels = "Panel A",
                                        realCoef = effectPanelA[2],
                                        sigCoef = round(quantile(D2$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D2$coefs, 0.975),3),
                                        N = "112",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D2$indicator),3),"]")),
                          D3 %>% mutate(models = "Model 3",
                                        panels = "Panel A",
                                        realCoef = effectPanelA[3],
                                        sigCoef = round(quantile(D3$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D3$coefs, 0.975),3),
                                        N = "114",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D3$indicator),3),"]")),
                          D4 %>% mutate(models = "Model 4",
                                        panels = "Panel A",
                                        realCoef = effectPanelA[4],
                                        sigCoef = round(quantile(D4$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D4$coefs, 0.975),3),
                                        N = "112",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D4$indicator),3),"]")),
                          D5 %>% mutate(models = "Model 5",
                                        panels = "Panel A",
                                        realCoef = effectPanelA[5],
                                        sigCoef = round(quantile(D5$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D5$coefs, 0.975),3),
                                        N = "114",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D5$indicator),3),"]")),
                          D6 %>% mutate(models = "Model 6",
                                        panels = "Panel A",
                                        realCoef = effectPanelA[6],
                                        sigCoef = round(quantile(D6$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D6$coefs, 0.975),3),
                                        N = "112",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D6$indicator),3),"]")))

dataFigPanelB = bind_rows(D7 %>% mutate(models = "Model 1",
                                        panels = "Panel B",
                                        realCoef = effectPanelB[1],
                                        sigCoef = round(quantile(D7$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D7$coefs, 0.975),3),
                                        N = "92",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D7$indicator),3),"]")),
                          D8 %>% mutate(models = "Model 2",
                                        panels = "Panel B",
                                        realCoef = effectPanelB[2],
                                        sigCoef = round(quantile(D8$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D8$coefs, 0.975),3),
                                        N = "76",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D8$indicator),3),"]")),
                          D9 %>% mutate(models = "Model 3",
                                        panels = "Panel B",
                                        realCoef = effectPanelB[3],
                                        sigCoef = round(quantile(D9$coefs, 0.025),3),
                                        sigCoef_upper = round(quantile(D9$coefs, 0.975),3),
                                        N = "92",
                                        sampleSize = "N = ",
                                        RIPval = paste0("[",round(mean(D9$indicator),3),"]")),
                          D10 %>% mutate(models = "Model 4",
                                         panels = "Panel B",
                                         realCoef = effectPanelB[4],
                                         sigCoef = round(quantile(D10$coefs, 0.025),3),
                                         sigCoef_upper = round(quantile(D10$coefs, 0.975),3),
                                         N = "76",
                                         sampleSize = "N = ",
                                         RIPval = paste0("[",round(mean(D10$indicator),3),"]")),
                          D11 %>% mutate(models = "Model 5",
                                         panels = "Panel B",
                                         realCoef = effectPanelB[5],
                                         sigCoef = round(quantile(D11$coefs, 0.025),3),
                                         sigCoef_upper = round(quantile(D11$coefs, 0.975),3),
                                         N = "92",
                                         sampleSize = "N = ",
                                         RIPval = paste0("[",round(mean(D11$indicator),3),"]")),
                          D12 %>% mutate(models = "Model 6",
                                         panels = "Panel B",
                                         realCoef = effectPanelB[6],
                                         sigCoef = round(quantile(D12$coefs, 0.025),3),
                                         sigCoef_upper = round(quantile(D12$coefs, 0.975),3),
                                         N = "76",
                                         sampleSize = "N = ",
                                         RIPval = paste0("[",round(mean(D12$indicator),3),"]")))
dataFig = dataFigPanelA

histFigPanelA = ggplot(data = dataFig) +
  geom_histogram(aes(coefs, fill = (coefs<sigCoef | coefs>sigCoef_upper)), bins = 150) +
  scale_fill_manual(breaks = c("TRUE","FALSE"), values = c("slategrey",'lightgrey')) + 
  # geom_vline(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
  #            aes(xintercept = realCoef), color = "royalblue4") + 
  geom_segment(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
               aes(xend = realCoef, yend = 300, x = realCoef, y=0), color = "royalblue4") + 
  
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300), 
            hjust = -0.3, color = "royalblue4", parse = TRUE, label = expression(paste(hat(beta)," = "))) +
  geom_text(data = dataFig%>%filter(models=="Model 1")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 2")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -1, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 3")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 4")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.7, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 5")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.7, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 6")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.8, color = "royalblue4") +
  
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=280), 
            hjust = -0.1, color = "royalblue4", label = "RI P-Val = ") +
  geom_text(data = dataFig%>%filter(models=="Model 1")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 2")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 3")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 4")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 5")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.9, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 6")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  
  geom_segment(data = dataFig%>%select(models, panels, sigCoef)%>%distinct(),
               aes(xend = sigCoef, yend = 270, x = sigCoef, y=0), color = "red3") + 
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=260), 
            hjust = -0.2, color = "red3", parse = TRUE, label = expression(paste(beta[0.05]," = "))) +
  geom_text(data = dataFig%>%filter(models=="Model 1")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -2.8, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 2")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -4.8, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 3")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -3.5, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 4")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -2.4, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 5")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -2.3, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 6")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -3.2, color = "red3") +
  
  geom_text(data = dataFig%>%filter(models %in% c("Model 1","Model 2","Model 6"))%>%select(models, panels,N)%>%distinct(),
            aes(x = 1.46, y = 6, label = N)) +
  geom_text(data = dataFig%>%filter(models %in% c("Model 1","Model 2","Model 6"))%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 1.3, y = 6, label = sampleSize)) +
  geom_text(data = dataFig%>%filter(models %in% c("Model 3","Model 4"))%>%select(models, panels,N)%>%distinct(),
            aes(x = 0.34, y = 6, label = N)) + 
  geom_text(data = dataFig%>%filter(models %in% c("Model 3","Model 4"))%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.3, y = 6, label = sampleSize)) + 
  geom_text(data = dataFig%>%filter(models %in% c("Model 5"))%>%select(models, panels,N)%>%distinct(),
            aes(x = 0.9, y = 6, label = N)) + 
  geom_text(data = dataFig%>%filter(models %in% c("Model 5"))%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.78, y = 6, label = sampleSize)) + 
  facet_wrap(~ models, scales = "free",nrow = 2, ncol = 3) +
  xlab("Effect Size") +
  ylab("Frequency") + 
  theme_bw() + theme(legend.position = "none")


ggsave("Figure-SI13-Panel-A.pdf", histFigPanelA, width = 14, height = 8)

dataFig = dataFigPanelB

histFigPanelB = ggplot(data = dataFig) +
  geom_histogram(aes(coefs, fill = (coefs<sigCoef | coefs>sigCoef_upper)), bins = 150) +
  scale_fill_manual(breaks = c("TRUE","FALSE"), values = c("slategrey",'lightgrey')) + 
  # geom_vline(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
  #            aes(xintercept = realCoef), color = "royalblue4") + 
  geom_segment(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
               aes(xend = realCoef, yend = 300, x = realCoef, y=0), color = "royalblue4") + 
  
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300), 
            hjust = -0.3, color = "royalblue4", parse = TRUE, label = expression(paste(hat(beta)," = "))) +
  geom_text(data = dataFig%>%filter(models=="Model 1")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.75, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 2")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -1.1, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 3")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 4")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.7, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 5")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.7, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 6")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=300, label = realCoef), 
            hjust = -0.8, color = "royalblue4") +
  
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=280), 
            hjust = -0.1, color = "royalblue4", label = "RI P-Val = ") +
  geom_text(data = dataFig%>%filter(models=="Model 1")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.9, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 2")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 3")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 4")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 5")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models=="Model 6")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=280, label = RIPval), 
            hjust = -2.2, color = "royalblue4") +
  
  geom_segment(data = dataFig%>%filter(models %in% c("Model 1","Model 3","Model 4","Model 5"))%>%
                 select(models, panels, sigCoef)%>%distinct(),
               aes(xend = sigCoef, yend = 270, x = sigCoef, y=0), color = "red3") + 
  geom_segment(data = dataFig%>%filter(models %in% c("Model 2","Model 6"))%>%
                 select(models, panels, sigCoef_upper)%>%distinct(),
               aes(xend = sigCoef_upper, yend = 240, x = sigCoef_upper, y=0), color = "red3") + 
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=260), 
            hjust = -0.2, color = "red3", parse = TRUE, label = expression(paste(beta[0.05]," = "))) +
  geom_text(data = dataFig%>%filter(models=="Model 1")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -2.6, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 2")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -5.1, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 3")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -2.4, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 4")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -2.1, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 5")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -2.6, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 6")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=262, label = sigCoef), 
            hjust = -3.8, color = "red3") +
  
  geom_text(data = dataFig%>%filter(models %in% c("Model 1","Model 2","Model 6"))%>%select(models, panels,N)%>%distinct(),
            aes(x = 2.48, y = 6, label = N)) +
  geom_text(data = dataFig%>%filter(models %in% c("Model 1","Model 2","Model 6"))%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 2.25, y = 6, label = sampleSize)) +
  geom_text(data = dataFig%>%filter(models %in% c("Model 3","Model 4"))%>%select(models, panels,N)%>%distinct(),
            aes(x = 0.4, y = 6, label = N)) + 
  geom_text(data = dataFig%>%filter(models %in% c("Model 3","Model 4"))%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.35, y = 6, label = sampleSize)) + 
  geom_text(data = dataFig%>%filter(models %in% c("Model 5"))%>%select(models, panels,N)%>%distinct(),
            aes(x = 1, y = 6, label = N)) + 
  geom_text(data = dataFig%>%filter(models %in% c("Model 5"))%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.9, y = 6, label = sampleSize)) + 
  facet_wrap(~ models, scales = "free",nrow = 2, ncol = 3) +
  xlab("Effect Size") +
  ylab("Frequency") + 
  theme_bw() + theme(legend.position = "none")


ggsave("Figure-SI13-Panel-B.pdf", histFigPanelB, width = 14, height = 8)






