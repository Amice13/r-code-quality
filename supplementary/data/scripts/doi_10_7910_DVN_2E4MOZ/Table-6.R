########################################################
## This R-file produces Table 6 in the paper
## Panel A and Panel B will be out-sheeted separately
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "lfe", "fixest","doMC","foreach","haven","stats4","texreg","randomizr","broom",
                   "marginaleffects","grDevices","ggrepel", "modelsummary"))
library(tidyverse);library(lfe);library(fixest);library(doMC);library(foreach);library(haven)
library(stats4);library(texreg);library(randomizr);library(broom);library(marginaleffects);
library(grDevices);library(ggrepel);library(modelsummary)

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



######################################################################
## Outsheet tables
######################################################################

coefficients = c("id" = "Treatment",
                 "persig14" = "Performance",
                 "id:persig14" = "Treatment x Performance")

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R2", 2)

############################### 
#######     Panel A    ########
###############################

models = list()
models[["(1)"]] = fitPanelA1
models[["(2)"]] = fitPanelA2
models[["(3)"]] = fitPanelA3
models[["(4)"]] = fitPanelA4
models[["(5)"]] = fitPanelA5
models[["(6)"]] = fitPanelA6

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",
                        "Party advantage", "Low", "High", "Low", "High", "Low", "High",
                        'Covariates', 'yes',   'yes', 'yes',   'yes', 'yes',   'yes',
                        "RI Pval (Low Performance)", 
                        paste0("[",round(mean(D1$indicator),3),"]"),
                        paste0("[",round(mean(D2$indicator),3),"]"),
                        paste0("[",round(mean(D3$indicator),3),"]"),
                        paste0("[",round(mean(D4$indicator),3),"]"),
                        paste0("[",round(mean(D5$indicator),3),"]"),
                        paste0("[",round(mean(D6$indicator),3),"]"),
                        "ME (High Performance)",
                        as.character(round(marginA1$dydx[marginA1$persig14 == 1],3)), as.character(round(marginA2$dydx[marginA2$persig14 == 1],3)), 
                        as.character(round(marginA3$dydx[marginA3$persig14 == 1],3)), as.character(round(marginA4$dydx[marginA4$persig14 == 1],3)),
                        as.character(round(marginA5$dydx[marginA5$persig14 == 1],3)), as.character(round(marginA6$dydx[marginA6$persig14 == 1],3)),
                        "SE (High Performance)",
                        paste0("(",round(marginA1$std.error[marginA1$persig14 == 1],3),")"), paste0("(",round(marginA2$std.error[marginA2$persig14 == 1],3),")"), 
                        paste0("(",round(marginA3$std.error[marginA3$persig14 == 1],3),")"), paste0("(",round(marginA4$std.error[marginA4$persig14 == 1],3),")"),
                        paste0("(",round(marginA5$std.error[marginA5$persig14 == 1],3),")"), paste0("(",round(marginA6$std.error[marginA6$persig14 == 1],3),")"))

attr(rows, 'position') <- c(7,8,9,10,11)

modelsummary(models, 
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = .01),
             coef_map = coefficients, 
             gof_map = gm,
             add_rows = rows, 
             title = 'Panel A: unconditional sample')

############################### 
#######     Panel B    ########
###############################

models = list()
models[["(1)"]] = fitPanelB1
models[["(2)"]] = fitPanelB2
models[["(3)"]] = fitPanelB3
models[["(4)"]] = fitPanelB4
models[["(5)"]] = fitPanelB5
models[["(6)"]] = fitPanelB6

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",
                        "Party advantage", "Low", "High", "Low", "High", "Low", "High",
                        'Covariates', 'yes',   'yes', 'yes',   'yes', 'yes',   'yes',
                        "RI Pval (Low Performance)", 
                        paste0("[",round(mean(D7$indicator),3),"]"),
                        paste0("[",round(mean(D8$indicator),3),"]"),
                        paste0("[",round(mean(D9$indicator),3),"]"),
                        paste0("[",round(mean(D10$indicator),3),"]"),
                        paste0("[",round(mean(D11$indicator),3),"]"),
                        paste0("[",round(mean(D12$indicator),3),"]"),
                        "ME (High Performance)",
                        as.character(round(marginB1$dydx[marginB1$persig14 == 1],3)), as.character(round(marginB2$dydx[marginB2$persig14 == 1],3)), 
                        as.character(round(marginB3$dydx[marginB3$persig14 == 1],3)), as.character(round(marginB4$dydx[marginB4$persig14 == 1],3)),
                        as.character(round(marginB5$dydx[marginB5$persig14 == 1],3)), as.character(round(marginB6$dydx[marginB6$persig14 == 1],3)),
                        "SE (High Performance)",
                        paste0("(",round(marginB1$std.error[marginB1$persig14 == 1],3),")"), paste0("(",round(marginB2$std.error[marginB2$persig14 == 1],3),")"), 
                        paste0("(",round(marginB3$std.error[marginB3$persig14 == 1],3),")"), paste0("(",round(marginB4$std.error[marginB4$persig14 == 1],3),")"),
                        paste0("(",round(marginB5$std.error[marginB5$persig14 == 1],3),")"), paste0("(",round(marginB6$std.error[marginB6$persig14 == 1],3),")"))

attr(rows, 'position') <- c(7,8,9,10,11)

modelsummary(models, 
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = .01),
             coef_map = coefficients, 
             gof_map = gm,
             add_rows = rows, 
             title = 'Panel B: sample is conditional of winning party nomination')
