########################################################
## This R-file produces Figure 10 in SI
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "lfe", "fixest","doMC","foreach","haven","stats4","texreg","randomizr","broom",
                   "marginaleffects","grDevices","ggrepel"))
library(tidyverse);library(lfe);library(fixest);library(doMC);library(foreach);library(haven)
library(stats4);library(texreg);library(randomizr);library(broom);library(marginaleffects);
library(grDevices);library(ggrepel)

##
set.seed(123)
## set working directory as root = ""

dataRaw = read_dta("10_Data_analysis_final.dta")
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
dataPanelA$coun_IND
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
## The simulation process takes a long time

effectPanelA = c(tidy(fitPanelA1)[1,2][[1]],tidy(fitPanelA2)[1,2][[1]],tidy(fitPanelA3)[1,2][[1]],tidy(fitPanelA4)[1,2][[1]],
                 tidy(fitPanelA5)[1,2][[1]],tidy(fitPanelA6)[1,2][[1]],tidy(fitPanelA7)[1,2][[1]],tidy(fitPanelA8)[1,2][[1]])
effectPanelB = c(tidy(fitPanelB1)[1,2][[1]],tidy(fitPanelB2)[1,2][[1]],tidy(fitPanelB3)[1,2][[1]],tidy(fitPanelB4)[1,2][[1]],
                 tidy(fitPanelB5)[1,2][[1]],tidy(fitPanelB6)[1,2][[1]],tidy(fitPanelB7)[1,2][[1]],tidy(fitPanelB8)[1,2][[1]])

effectPanel = c(effectPanelA,effectPanelB)

## n = 1:16 represents model (1) to (8) in Panel A, then model (1) to (8) in Panel B
## in the same order as effect size vector `effectPanel`
registerDoMC(detectCores() - 1)
foreach::foreach(n = 1:16) %do% {
  set.seed(123)
  ## Initiate vectors to score statistics
  SimID = c()
  coefs = c()
  ses = c()
  tstats = c()
  pvals = c()
  ci_lowers = c()
  ci_uppers = c()
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
    if(n==1){
      fitSim = feols(win_again_ofall_noind ~ treatment | master_district, data = dataSim,
                     cluster = ~master_district, weights = ~wid)}else if(n==2){
                       fitSim = feols(as.formula(paste0("win_again_ofall_noind ~ treatment + ",nocovariates, " | master_district")), 
                                      data = dataSim,cluster = ~master_district, weights = ~wid)}else if(n%in%c(3,5,7)){
                                        fitSim = feols(win_again_ofall_noind ~ treatment * persig14 | master_district, data = dataSim,
                                                       cluster = ~master_district, weights = ~wid)}else if(n%in%c(4,6,8)){
                                                         fitSim = feols(as.formula(paste0("win_again_ofall_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
                                                                        data = dataSim, cluster = ~master_district, weights = ~wid)}else if(n==9){
                                                                          fitSim = feols(win_again_noind ~ treatment | master_district, data = dataSim,
                                                                                         cluster = ~master_district, weights = ~wid)}else if(n==10){
                                                                                           fitSim = feols(as.formula(paste0("win_again_noind ~ treatment + ",nocovariates, " | master_district")), 
                                                                                                          data = dataSim,cluster = ~master_district, weights = ~wid)}else if(n%in%c(11,13,15)){
                                                                                                            fitSim = feols(win_again_noind ~ treatment * persig14 | master_district, data = dataSim,
                                                                                                                           cluster = ~master_district, weights = ~wid)}else if(n%in%c(12,14,16)){
                                                                                                                             fitSim = feols(as.formula(paste0("win_again_noind ~ treatment * persig14 + ",nocovariates, " | master_district")), 
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
  simResult$indicator = (abs(simResult$coefs) > abs(effectPanel[n]))
  ## Outsheet simulation results
  assign(paste0("D",n), simResult)}


#######################################################################
## Figures of the Randomized Inference Simulations
#######################################################################


effectPanelA = c(round(tidy(fitPanelA1)[1,2][[1]],3),round(tidy(fitPanelA2)[1,2][[1]],3),round(tidy(fitPanelA3)[1,2][[1]],3),round(tidy(fitPanelA4)[1,2][[1]],3),
                 round(tidy(fitPanelA5)[1,2][[1]],3),round(tidy(fitPanelA6)[1,2][[1]],3),round(tidy(fitPanelA7)[1,2][[1]],3),round(tidy(fitPanelA8)[1,2][[1]],3))
effectPanelB = c(round(tidy(fitPanelB1)[1,2][[1]],3),round(tidy(fitPanelB2)[1,2][[1]],3),round(tidy(fitPanelB3)[1,2][[1]],3),round(tidy(fitPanelB4)[1,2][[1]],3),
                 round(tidy(fitPanelB5)[1,2][[1]],3),round(tidy(fitPanelB6)[1,2][[1]],3),round(tidy(fitPanelB7)[1,2][[1]],3),round(tidy(fitPanelB8)[1,2][[1]],3))

dataFig = bind_rows(D4 %>% mutate(models = "Model 4",
                                  panels = "Panel A",
                                  realCoef = effectPanelA[4],
                                  sigCoef = round(quantile(D4$coefs, 0.025),3),
                                  sigCoef_upper = round(quantile(D4$coefs, 0.975),3),
                                  N = "354",
                                  sampleSize = "N = ",
                                  RIPval = paste0("[",round(mean(D4$indicator),3),"]")),
                    D6 %>% mutate(models = "Model 6",
                                  panels = "Panel A",
                                  realCoef = effectPanelA[6],
                                  sigCoef = round(quantile(D6$coefs, 0.025),3),
                                  sigCoef_upper = round(quantile(D6$coefs, 0.975),3),
                                  N = "166",
                                  sampleSize = "N = ",
                                  RIPval = paste0("[",round(mean(D6$indicator),3),"]")),
                    D8 %>% mutate(models = "Model 8",
                                  panels = "Panel A",
                                  realCoef = effectPanelA[8],
                                  sigCoef = round(quantile(D8$coefs, 0.025),3),
                                  sigCoef_upper = round(quantile(D8$coefs, 0.975),3),
                                  N = "188",
                                  sampleSize = "N = ",
                                  RIPval = paste0("[",round(mean(D8$indicator),3),"]")),
                    D12 %>% mutate(models = "Model 4",
                                   panels = "Panel B",
                                   realCoef = effectPanelB[4],
                                   sigCoef = round(quantile(D12$coefs, 0.025),3),
                                   sigCoef_upper = round(quantile(D12$coefs, 0.975),3),
                                   N = "168",
                                   sampleSize = "N = ",
                                   RIPval = paste0("[",round(mean(D12$indicator),3),"]")),
                    D14 %>% mutate(models = "Model 6",
                                   panels = "Panel B",
                                   realCoef = effectPanelB[6],
                                   sigCoef = round(quantile(D14$coefs, 0.025),3),
                                   sigCoef_upper = round(quantile(D14$coefs, 0.975),3),
                                   N = "92",
                                   sampleSize = "N = ",
                                   RIPval = paste0("[",round(mean(D14$indicator),3),"]")),
                    D16 %>% mutate(models = "Model 8",
                                   panels = "Panel B",
                                   realCoef = effectPanelB[8],
                                   sigCoef = round(quantile(D16$coefs, 0.025),3),
                                   sigCoef_upper = round(quantile(D16$coefs, 0.975),3),
                                   N = "76",
                                   sampleSize = "N = ",
                                   RIPval = paste0("[",round(mean(D16$indicator),3),"]")))

histFig = ggplot(data = dataFig) +
  geom_histogram(aes(coefs, fill = (coefs<sigCoef | coefs>sigCoef_upper)), bins = 150) +
  scale_fill_manual(breaks = c("TRUE","FALSE"), values = c("slategrey",'lightgrey')) + 
  # geom_vline(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
  #            aes(xintercept = realCoef), color = "royalblue4") + 
  geom_segment(data = dataFig%>%filter(panels=="Panel A")%>%select(models, panels, realCoef)%>%distinct(),
               aes(xend = realCoef, yend = 400, x = realCoef, y=0), color = "royalblue4") + 
  geom_segment(data = dataFig%>%filter(panels=="Panel B")%>%select(models, panels, realCoef)%>%distinct(),
               aes(xend = realCoef, yend = 250, x = realCoef, y=0), color = "royalblue4") + 
  
  geom_text(data = dataFig%>%filter(panels=="Panel A")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=400), 
            hjust = -0.3, color = "royalblue4", parse = TRUE, label = expression(paste(hat(beta)," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel A")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=400, label = realCoef), 
            hjust = -0.7, color = "royalblue4") +
  
  geom_text(data = dataFig%>%filter(panels=="Panel A")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=370), 
            hjust = -0.1, color = "royalblue4", label = "RI P-Val = ") +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models!="Model 8")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=370, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models=="Model 8")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=370, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 8")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250), 
            hjust = -0.25, color = "royalblue4", parse = TRUE, label = expression(paste(hat(beta)," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 8")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250, label = realCoef), hjust = -0.65, color = "royalblue4") +
  
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 8")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=230), 
            hjust = -0.08, color = "royalblue4", label = "RI P-Val = ") +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 8")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=230, label = RIPval), hjust = -2.1, color = "royalblue4") +
  
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 6")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250), 
            hjust = -0.9, color = "royalblue4", parse = TRUE, label = expression(paste(hat(beta)," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 6")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250, label = realCoef), hjust = -1.05, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 6")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=230), 
            hjust = -0.3, color = "royalblue4", label = "RI P-Val = ") +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 6")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=230, label = RIPval), hjust = -2.1, color = "royalblue4") +
  
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 4")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250), 
            hjust = -1.25, color = "royalblue4", parse = TRUE, label = expression(paste(hat(beta)," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 4")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250, label = realCoef), hjust = -1.25, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 4")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=230), 
            hjust = -0.43, color = "royalblue4", label = "RI P-Val = ") +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models=="Model 4")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=230, label = RIPval), hjust = -2.4, color = "royalblue4") +
  
  
  geom_segment(data = dataFig%>%filter(panels=="Panel A")%>%select(models, panels, sigCoef)%>%distinct(),
               aes(xend = sigCoef, yend = 350, x = sigCoef, y=0), color = "red3") + 
  geom_segment(data = dataFig%>%filter(panels=="Panel B")%>%select(models, panels, sigCoef)%>%distinct(),
               aes(xend = sigCoef, yend = 200, x = sigCoef, y=0), color = "red3") + 
  
  
  geom_text(data = dataFig%>%filter(panels=="Panel A")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=340), 
            hjust = -0.2, color = "red3", parse = TRUE, label = expression(paste(beta[0.05]," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models=="Model 4")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=342, label = sigCoef), 
            hjust = -1.1, color = "red3") +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models=="Model 6")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=342, label = sigCoef), 
            hjust = -1.3, color = "red3") +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models=="Model 8")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=342, label = sigCoef), 
            hjust = -1.55, color = "red3") +
  
  geom_text(data = dataFig%>%filter(panels=="Panel B", models == "Model 4")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=212), 
            hjust = -0.6, color = "red3", parse = TRUE, label = expression(paste(beta[0.05]," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models == "Model 4")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=213, label = sigCoef), hjust = -1.05, color = "red3") +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models == "Model 6")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=212), 
            hjust = -0.4, color = "red3", parse = TRUE, label = expression(paste(beta[0.05]," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models == "Model 6")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=213, label = sigCoef), hjust = -1, color = "red3") +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models == "Model 8")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=212), 
            hjust = -0.2, color = "red3", parse = TRUE, label = expression(paste(beta[0.05]," = "))) +
  geom_text(data = dataFig%>%filter(panels=="Panel B", models == "Model 8")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=213, label = sigCoef), hjust = -2.2, color = "red3") +
  
  geom_text(data = dataFig%>%filter(models == "Model 4")%>%select(models, panels,N)%>%distinct(),
            aes(x = 0.48, y = 6, label = N)) +
  geom_text(data = dataFig%>%filter(models == "Model 4")%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.41, y = 6, label = sampleSize)) +
  geom_text(data = dataFig%>%filter(models != "Model 4")%>%select(models, panels,N)%>%distinct(),
            aes(x = 0.83, y = 6, label = N)) + 
  geom_text(data = dataFig%>%filter(models != "Model 4")%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.72, y = 6, label = sampleSize)) + 
  facet_grid(panels ~ models, scales = "free") +
  xlab("Effect Size") +
  ylab("Frequency") + 
  theme_bw() + theme(legend.position = "none")


ggsave("Figure-SI10.pdf", histFig, width = 14, height = 8)
