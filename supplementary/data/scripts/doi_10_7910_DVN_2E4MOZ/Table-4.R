########################################################
## This R-file produces Table 4 in the paper
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

## Create dataset for each model

model13 = dataRaw %>% filter(!(is.na(run_again_noind)))
model24 = dataRaw %>% filter(!(is.na(run_again_noind)))  %>% 
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
model5 = dataRaw %>% filter(!(is.na(run_again_noind)),
                            PA3A == 0)
model6 = dataRaw %>% filter(!(is.na(run_again_noind)),
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
model7 = dataRaw %>% filter(!(is.na(run_again_noind)),
                            PA3A == 1)
model8 = dataRaw %>% filter(!(is.na(run_again_noind)),
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


## Run all models
fitPanelA1 = feols(run_again_noind ~ id | master_district, data = model13,
                   cluster = ~master_district, weights = ~wid)
fitPanelA2 = feols(as.formula(paste0("run_again_noind ~ id + ",nocovariates, " | master_district")), 
                   data = model24,
                   cluster = ~master_district, weights = ~wid)
fitPanelA3 = feols(run_again_noind ~ id * persig14 | master_district, data = model13,
                   cluster = ~master_district, weights = ~wid)
fitPanelA4 = feols(as.formula(paste0("run_again_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model24, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA5 = feols(run_again_noind ~ id * persig14 | master_district, 
                   data = model5,
                   cluster = ~master_district, weights = ~wid)
fitPanelA6 = feols(as.formula(paste0("run_again_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model6, 
                   cluster = ~master_district, weights = ~wid)
fitPanelA7 = feols(run_again_noind ~ id * persig14 | master_district, 
                   data = model7,
                   cluster = ~master_district, weights = ~wid)
fitPanelA8 = feols(as.formula(paste0("run_again_noind ~ id * persig14 + ",nocovariates, " | master_district")), 
                   data = model8, 
                   cluster = ~master_district, weights = ~wid)

## Get marginal effects for high performance group
marginA3 = marginaleffects(fitPanelA3, newdata = datagrid(persig14 = model13$persig14), variables = "id")
marginA4 = marginaleffects(fitPanelA4, newdata = datagrid(persig14 = model24$persig14), variables = "id")
marginA5 = marginaleffects(fitPanelA5, newdata = datagrid(persig14 = model5$persig14), variables = "id")
marginA6 = marginaleffects(fitPanelA6, newdata = datagrid(persig14 = model6$persig14), variables = "id")
marginA7 = marginaleffects(fitPanelA7, newdata = datagrid(persig14 = model7$persig14), variables = "id")
marginA8 = marginaleffects(fitPanelA8, newdata = datagrid(persig14 = model8$persig14), variables = "id")


#######################################################################
## Collect and calculate RI P-vals

effectPanelA = c(tidy(fitPanelA1)[1,2][[1]],tidy(fitPanelA2)[1,2][[1]],tidy(fitPanelA3)[1,2][[1]],tidy(fitPanelA4)[1,2][[1]],
                 tidy(fitPanelA5)[1,2][[1]],tidy(fitPanelA6)[1,2][[1]],tidy(fitPanelA7)[1,2][[1]],tidy(fitPanelA8)[1,2][[1]])

## n = 1:8 represents model (1) to (8) 
## in the same order as effect size vector `effectPanelA`
registerDoMC(detectCores()-1)
foreach::foreach(n=1:8)%do%{
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
    if(n==1){dataSim=model13}else if(n==2){dataSim=model24}else if(n==3){dataSim=model13}else if(n==4){
      dataSim=model24}else if(n==5){dataSim=model5}else if(n==6){dataSim=model6
      }else if(n==7){dataSim=model7}else if(n==8){dataSim=model8}
    
    
    ## random assign treatment, based on original treatment assignment probability in each district
    dataSim$treatment = block_ra(blocks = as.vector(dataSim$master_district),
                                 block_prob = pull(dataSim %>%
                                                     select(master_district,prob) %>% 
                                                     distinct() %>% 
                                                     arrange(master_district), prob))
    if(n==1){fitSim = feols(run_again_noind ~ treatment | master_district, data = dataSim,
                            cluster = ~master_district, weights = ~wid)
    }else if(n==2){fitSim = feols(as.formula(paste0("run_again_noind ~ treatment + ",nocovariates, " | master_district")), 
                                  data = dataSim,cluster = ~master_district, weights = ~wid)
    }else if(n%in%c(3,5,7)){fitSim = feols(run_again_noind ~ treatment * persig14 | master_district, data = dataSim,
                                           cluster = ~master_district, weights = ~wid)
    }else if(n%in%c(4,6,8)){fitSim = feols(as.formula(paste0("run_again_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                           data = dataSim,cluster = ~master_district, weights = ~wid)}
    
    sumSim = tidy(fitSim)
    coefs = c(coefs, sumSim$estimate[sumSim$term == "treatment"])
    ses = c(ses, sumSim$std.error[sumSim$term == "treatment"])
    tstats = c(tstats, sumSim$statistic[sumSim$term == "treatment"])
    pvals = c(pvals, sumSim$p.value[sumSim$term == "treatment"])
    #ci_lowers = c(ci_lowers, sumSim$conf.low[sumSim$term == "treatment"])
    #ci_uppers = c(ci_uppers, sumSim$conf.high[sumSim$term == "treatment"])
    obs = c(obs, fitSim$nobs)
    r2s = c(r2s, glance(fitSim)$r.squared)}
  
  print(paste0("D", n," dataset has been finished."))
  simResult = data.frame(SimID, coefs, ses, tstats, pvals, obs, r2s)
  ## Calculate the randomization inference P-value
  simResult$indicator = (abs(simResult$coefs) > abs(effectPanelA[n]))
  
  ## Outsheet simulation results
  assign(paste0("D",n), simResult)}


######################################################################
## Outsheet table 3
######################################################################

coefficients = c("id" = "Treatment",
                 "persig14" = "Performance",
                 "id:persig14" = "Treatment x Performance")

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R2", 2)


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
             stars = c('+' = .1, '*' = .05, '**' = .01),
             coef_map = coefficients, 
             gof_map = gm,
             add_rows = rows, 
             title = 'DV: Ran again.')
