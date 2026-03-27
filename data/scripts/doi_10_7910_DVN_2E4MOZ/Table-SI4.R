########################################################
## This R-file produces Table 4 in SI 
## Warning: the simulation takes long time
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "lfe", "fixest","doMC","foreach","haven","stats4","texreg","randomizr","broom",
                   "marginaleffects","grDevices","ggrepel","modelsummary"))
library(tidyverse);library(lfe);library(fixest);library(doMC);library(foreach);library(haven)
library(stats4);library(texreg);library(randomizr);library(broom);library(marginaleffects);
library(grDevices);library(ggrepel);library(modelsummary)

##
set.seed(123)
## set working directory as root = ""

dataRaw = read_dta("10_Data_analysis_final.dta")
dataPanelA = dataRaw %>% filter(!is.na(win_again_ofall_noind_b)) %>% mutate(master_district = factor(master_district),
                                                                            prob = 1/wid)
dataPanelB = dataRaw %>% filter(!is.na(win_again_noind_b)) %>% mutate(master_district = factor(master_district),
                                                                      prob = 1/wid)


## Panel A model (1)(3)(5)(7): simple filter, no covariates
## Panel B model (1)(3)(5)(7): simple filter, no covariates
model5PanelA = dataPanelA %>% filter(PA3B == 0)
model7PanelA = dataPanelA %>% filter(PA3B == 2)
model5PanelB = dataPanelB %>% filter(PA3B == 0)
model7PanelB = dataPanelB %>% filter(PA3B == 2)

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
  filter(PA3B == 0) %>%
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
  filter(PA3B == 0) %>%
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
  filter(PA3B == 2) %>%
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
  filter(PA3B == 2) %>%
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

controls = c("master_mandate_sd",'coun_NRM_sd','coun_age_sd', 'coun_asst_motor_sd', 'coun_terms_sd', 'coun_speaker_sd', 
             'log_pop_ea_sd', 'literacy_share_ea_sd', 'elf_ea_sd', 'poverty_census_ea_sd', 'agr_share_ea_sd')
nocovariates = paste0(paste0(controls,collapse = " * id + "), " * id")

## Pay attention to the following changes on model specification
## 1. add district fixed effect, cluster se on district level
## 2. add a weight that is inverse treatment assignment
## 3. variable district need to be converted to factor in order to allow margineffects function to work
## 4. have to use fixest feols instead of lfe felm because margineffects doesn't accept felm object.

## Run all models
fitPanelA1 = feols(win_again_ofall_noind_b ~ id | master_district, data = dataPanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA2 = feols(as.formula(paste0("win_again_ofall_noind_b ~ id + ",nocovariates, " | master_district")), 
                   data = model24PanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA3 = feols(win_again_ofall_noind_b ~ id * persig14 | master_district, data = dataPanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA4 = feols(as.formula(paste0("win_again_ofall_noind_b ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model24PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA5 = feols(win_again_ofall_noind_b ~ id * persig14 | master_district, 
                   data = model5PanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA6 = feols(as.formula(paste0("win_again_ofall_noind_b ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model6PanelA, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA7 = feols(win_again_ofall_noind_b ~ id * persig14 | master_district, 
                   data = model7PanelA,
                   cluster = ~master_district, weights = ~wid)
fitPanelA8 = feols(as.formula(paste0("win_again_ofall_noind_b ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model8PanelA, 
                   cluster = ~master_district, weights = ~wid)

fitPanelB1 = feols(win_again_noind_b ~ id | master_district, data = dataPanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB2 = feols(as.formula(paste0("win_again_noind_b ~ id + ",nocovariates, " | master_district")), 
                   data = model24PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB3 = feols(win_again_noind_b ~ id * persig14 | master_district, data = dataPanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB4 = feols(as.formula(paste0("win_again_noind_b ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model24PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB5 = feols(win_again_noind_b ~ id * persig14 | master_district, 
                   data = model5PanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB6 = feols(as.formula(paste0("win_again_noind_b ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model6PanelB, 
                   cluster = ~master_district, weights = ~wid)
fitPanelB7 = feols(win_again_noind_b ~ id * persig14 | master_district, 
                   data = model7PanelB,
                   cluster = ~master_district, weights = ~wid)
fitPanelB8 = feols(as.formula(paste0("win_again_noind_b ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model8PanelB, 
                   cluster = ~master_district, weights = ~wid)

#######################################################################
## Get Marginal effects for High performance

## Panel A and B model (1)(2) do not have interaction item.
marginA3 = marginaleffects(fitPanelA3, newdata = datagrid(persig14 = dataPanelA$persig14), variables = "id")
marginA4 = marginaleffects(fitPanelA4, newdata = datagrid(persig14 = model24PanelA$persig14), variables = "id")
marginA5 = marginaleffects(fitPanelA5, newdata = datagrid(persig14 = model5PanelA$persig14), variables = "id")
marginA6 = marginaleffects(fitPanelA6, newdata = datagrid(persig14 = model6PanelA$persig14), variables = "id")
marginA7 = marginaleffects(fitPanelA7, newdata = datagrid(persig14 = model7PanelA$persig14), variables = "id")
marginA8 = marginaleffects(fitPanelA8, newdata = datagrid(persig14 = model8PanelA$persig14), variables = "id")

marginB3 = marginaleffects(fitPanelB3, newdata = datagrid(persig14 = dataPanelB$persig14), variables = "id")
marginB4 = marginaleffects(fitPanelB4, newdata = datagrid(persig14 = model24PanelB$persig14), variables = "id")
marginB5 = marginaleffects(fitPanelB5, newdata = datagrid(persig14 = model5PanelB$persig14), variables = "id")
marginB6 = marginaleffects(fitPanelB6, newdata = datagrid(persig14 = model6PanelB$persig14), variables = "id")
marginB7 = marginaleffects(fitPanelB7, newdata = datagrid(persig14 = model7PanelB$persig14), variables = "id")
marginB8 = marginaleffects(fitPanelB8, newdata = datagrid(persig14 = model8PanelB$persig14), variables = "id")

#######################################################################
## Collect and calculate RI P-vals

effectPanelA = c(tidy(fitPanelA1)[1,2][[1]],tidy(fitPanelA2)[1,2][[1]],tidy(fitPanelA3)[1,2][[1]],tidy(fitPanelA4)[1,2][[1]],
                 tidy(fitPanelA5)[1,2][[1]],tidy(fitPanelA6)[1,2][[1]],tidy(fitPanelA7)[1,2][[1]],tidy(fitPanelA8)[1,2][[1]])
effectPanelB = c(tidy(fitPanelB1)[1,2][[1]],tidy(fitPanelB2)[1,2][[1]],tidy(fitPanelB3)[1,2][[1]],tidy(fitPanelB4)[1,2][[1]],
                 tidy(fitPanelB5)[1,2][[1]],tidy(fitPanelB6)[1,2][[1]],tidy(fitPanelB7)[1,2][[1]],tidy(fitPanelB8)[1,2][[1]])

effectPanel = c(effectPanelA,effectPanelB)

## n = 1:16 represents model (1) to (8) in Panel A, then model (1) to (8) in Panel B
## in the same order as effect size vector `effectPanel`
registerDoMC(detectCores()-1)
foreach::foreach(n=1:16)%do%{
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
    if(n==1){dataSim=dataPanelA}else if(n==2){dataSim=model24PanelA}else if(n==3){dataSim=dataPanelA}else if(n==4){
      dataSim=model24PanelA}else if(n==5){dataSim=model5PanelA}else if(n==6){dataSim=model6PanelA
      }else if(n==7){dataSim=model7PanelA}else if(n==8){dataSim=model8PanelA}else if(n==9){dataSim=dataPanelB
      }else if(n==10){dataSim=model24PanelB}else if(n==11){dataSim=dataPanelB}else if(n==12){dataSim=model24PanelB
      }else if(n==13){dataSim=model5PanelB}else if(n==14){dataSim=model6PanelB}else if(n==15){dataSim=model7PanelB
      }else if(n==16){dataSim=model8PanelB}
    
    ## random assign treatment, based on original treatment assignment probability in each district
    dataSim$treatment = block_ra(blocks = as.vector(dataSim$master_district),
                                 block_prob = pull(dataSim %>%
                                                     select(master_district,prob) %>% 
                                                     distinct() %>% 
                                                     arrange(master_district), prob))
    if(n==1){fitSim = feols(win_again_ofall_noind_b ~ treatment | master_district, data = dataSim,
                            cluster = ~master_district, weights = ~wid)
    }else if(n==2){fitSim = feols(as.formula(paste0("win_again_ofall_noind_b ~ treatment + ",nocovariates, " | master_district")), 
                                  data = dataSim,cluster = ~master_district, weights = ~wid)
    }else if(n%in%c(3,5,7)){fitSim = feols(win_again_ofall_noind_b ~ treatment * persig14 | master_district, data = dataSim,
                                           cluster = ~master_district, weights = ~wid)
    }else if(n%in%c(4,6,8)){fitSim = feols(as.formula(paste0("win_again_ofall_noind_b ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                           data = dataSim, cluster = ~master_district, weights = ~wid)
    }else if(n==9){fitSim = feols(win_again_noind_b ~ treatment | master_district, data = dataSim,
                                  cluster = ~master_district, weights = ~wid)
    }else if(n==10){fitSim = feols(as.formula(paste0("win_again_noind_b ~ treatment + ",nocovariates, " | master_district")), 
                                   data = dataSim,cluster = ~master_district, weights = ~wid)
    }else if(n%in%c(11,13,15)){fitSim = feols(win_again_noind_b ~ treatment * persig14 | master_district, data = dataSim,
                                              cluster = ~master_district, weights = ~wid)
    }else if(n%in%c(12,14,16)){fitSim = feols(as.formula(paste0("win_again_noind_b ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                              data = dataSim, cluster = ~master_district, weights = ~wid)}
    
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
  simResult$indicator = (abs(coefs) > abs(effectPanel[n]))
  
  ## Outsheet simulation results
  assign(paste0("D",n), simResult)}

#######################################################################
## Outsheet table 3 SI
#######################################################################

coefficients = c("id" = "Treatment",
                    "persig14" = "Performance",
                    "id:persig14" = "Treatment x Performance")


gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R<sup>2</sup>", 2)

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
models[["(7)"]] = fitPanelA7
models[["(8)"]] = fitPanelA8

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",  ~"(7)",  ~"(8)",
                        'Covariates', 'no',   'yes', 'no',   'yes', 'no',   'yes', 'no',   'yes',
                        "RI Pval (Low Performance)", 
                        paste0("[",round(mean(D1$indicator),3),"]"),
                        paste0("[",round(mean(D2$indicator),3),"]"),
                        paste0("[",round(mean(D3$indicator),3),"]"),
                        paste0("[",round(mean(D4$indicator),3),"]"),
                        paste0("[",round(mean(D5$indicator),3),"]"),
                        paste0("[",round(mean(D6$indicator),3),"]"),
                        paste0("[",round(mean(D7$indicator),3),"]"),
                        paste0("[",round(mean(D8$indicator),3),"]"),
                        "ME (High Performance)",
                        "", "", as.character(round(marginA3$dydx[marginA3$persig14 == 1],3)), as.character(round(marginA4$dydx[marginA4$persig14 == 1],3)),
                        as.character(round(marginA5$dydx[marginA5$persig14 == 1],3)), as.character(round(marginA6$dydx[marginA6$persig14 == 1],3)), 
                        as.character(round(marginA7$dydx[marginA7$persig14 == 1],3)), as.character(round(marginA8$dydx[marginA8$persig14 == 1],3)),
                        "SE (High Performance)",
                        "", "", paste0("(",round(marginA3$std.error[marginA3$persig14 == 1],3),")"), paste0("(",round(marginA4$std.error[marginA4$persig14 == 1],3),")"),
                        paste0("(",round(marginA5$std.error[marginA5$persig14 == 1],3),")"), paste0("(",round(marginA6$std.error[marginA6$persig14 == 1],3),")"), 
                        paste0("(",round(marginA7$std.error[marginA7$persig14 == 1],3),")"), paste0("(",round(marginA8$std.error[marginA8$persig14 == 1],3),")"))

attr(rows, 'position') <- c(7,8,9,10)

modelsummary(models, 
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
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
models[["(7)"]] = fitPanelB7
models[["(8)"]] = fitPanelB8

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",  ~"(7)",  ~"(8)",
                        'Covariates', 'no',   'yes', 'no',   'yes', 'no',   'yes', 'no',   'yes',
                        "RI Pval (Low Performance)", 
                        paste0("[",round(mean(D9$indicator),3),"]"),
                        paste0("[",round(mean(D10$indicator),3),"]"),
                        paste0("[",round(mean(D11$indicator),3),"]"),
                        paste0("[",round(mean(D12$indicator),3),"]"),
                        paste0("[",round(mean(D13$indicator),3),"]"),
                        paste0("[",round(mean(D14$indicator),3),"]"),
                        paste0("[",round(mean(D15$indicator),3),"]"),
                        paste0("[",round(mean(D16$indicator),3),"]"),
                        "ME (High Performance)",
                        "", "", as.character(round(marginB3$dydx[marginB3$persig14 == 1],3)), as.character(round(marginB4$dydx[marginB4$persig14 == 1],3)),
                        as.character(round(marginB5$dydx[marginB5$persig14 == 1],3)), as.character(round(marginB6$dydx[marginB6$persig14 == 1],3)), 
                        as.character(round(marginB7$dydx[marginB7$persig14 == 1],3)), as.character(round(marginB8$dydx[marginB8$persig14 == 1],3)),
                        "SE (High Performance)",
                        "", "", paste0("(",round(marginB3$std.error[marginB3$persig14 == 1],3),")"), paste0("(",round(marginB4$std.error[marginB4$persig14 == 1],3),")"),
                        paste0("(",round(marginB5$std.error[marginB5$persig14 == 1],3),")"), paste0("(",round(marginB6$std.error[marginB6$persig14 == 1],3),")"), 
                        paste0("(",round(marginB7$std.error[marginB7$persig14 == 1],3),")"), paste0("(",round(marginB8$std.error[marginB8$persig14 == 1],3),")"))

attr(rows, 'position') <- c(7,8,9,10)

modelsummary(models, 
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = coefficients, 
             gof_map = gm,
             add_rows = rows, 
             title = 'Panel B: sample is conditional of winning party nomination')

