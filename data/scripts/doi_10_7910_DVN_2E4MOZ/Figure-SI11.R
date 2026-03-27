########################################################
## This R-file produces Figure 11 in SI
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



#######################################################################
## Figures of the Randomized Inference Simulations
#######################################################################


effectPanelA = c(round(tidy(fitPanelA1)[1,2][[1]],3),round(tidy(fitPanelA2)[1,2][[1]],3),round(tidy(fitPanelA3)[1,2][[1]],3),round(tidy(fitPanelA4)[1,2][[1]],3),
                 round(tidy(fitPanelA5)[1,2][[1]],3),round(tidy(fitPanelA6)[1,2][[1]],3),round(tidy(fitPanelA7)[1,2][[1]],3),round(tidy(fitPanelA8)[1,2][[1]],3))

dataFig = bind_rows(D4 %>% mutate(models = "Model 4",
                                  panels = "Panel A",
                                  realCoef = effectPanelA[4],
                                  sigCoef = round(quantile(D4$coefs, 0.025),3),
                                  sigCoef_upper = round(quantile(D4$coefs, 0.975),3),
                                  N = "335",
                                  sampleSize = "N = ",
                                  RIPval = paste0("[",round(mean(D4$indicator),3),"]")),
                    D6 %>% mutate(models = "Model 6",
                                  panels = "Panel A",
                                  realCoef = effectPanelA[6],
                                  sigCoef = round(quantile(D6$coefs, 0.025),3),
                                  sigCoef_upper = round(quantile(D6$coefs, 0.975),3),
                                  N = "159",
                                  sampleSize = "N = ",
                                  RIPval = paste0("[",round(mean(D6$indicator),3),"]")),
                    D8 %>% mutate(models = "Model 8",
                                  panels = "Panel A",
                                  realCoef = effectPanelA[8],
                                  sigCoef = round(quantile(D8$coefs, 0.025),3),
                                  sigCoef_upper = round(quantile(D8$coefs, 0.975),3),
                                  N = "176",
                                  sampleSize = "N = ",
                                  RIPval = paste0("[",round(mean(D8$indicator),3),"]")))

histFig = ggplot(data = dataFig) +
  geom_histogram(aes(coefs, fill = (coefs<sigCoef | coefs>sigCoef_upper)), bins = 150) +
  scale_fill_manual(breaks = c("TRUE","FALSE"), values = c("slategrey",'lightgrey')) + 
  # geom_vline(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
  #            aes(xintercept = realCoef), color = "royalblue4") + 
  geom_segment(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
               aes(xend = realCoef, yend = 250, x = realCoef, y=0), color = "royalblue4") + 
  
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250), 
            hjust = -0.3, color = "royalblue4", parse = TRUE, label = expression(paste(hat(beta)," = "))) +
  geom_text(data = dataFig%>%filter(models=="Model 8")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250, label = realCoef), 
            hjust = -0.9, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(models!="Model 8")%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=250, label = realCoef), 
            hjust = -0.7, color = "royalblue4") +
  
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=230), 
            hjust = -0.1, color = "royalblue4", label = "RI P-Val = ") +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models=="Model 4")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=230, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models=="Model 6")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=230, label = RIPval), 
            hjust = -2.8, color = "royalblue4") +
  geom_text(data = dataFig%>%filter(panels=="Panel A", models=="Model 8")%>%select(models, panels, realCoef, RIPval)%>%distinct(),
            mapping = aes(x=realCoef, y=230, label = RIPval), 
            hjust = -1.8, color = "royalblue4") +
  
  
  geom_segment(data = dataFig%>%select(models, panels, sigCoef)%>%distinct(),
               aes(xend = sigCoef, yend = 220, x = sigCoef, y=0), color = "red3") + 
  
  
  geom_text(data = dataFig%>%select(models, panels, realCoef)%>%distinct(),
            mapping = aes(x=realCoef, y=210), 
            hjust = -0.2, color = "red3", parse = TRUE, label = expression(paste(beta[0.05]," = "))) +
  geom_text(data = dataFig%>%filter(models=="Model 4")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=212, label = sigCoef), 
            hjust = -1.6, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 6")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=212, label = sigCoef), 
            hjust = -2.3, color = "red3") +
  geom_text(data = dataFig%>%filter(models=="Model 8")%>%select(models, panels, sigCoef)%>%distinct(),
            mapping = aes(x=sigCoef, y=212, label = sigCoef), 
            hjust = -2.6, color = "red3") +
  
  geom_text(data = dataFig%>%filter(models == "Model 4")%>%select(models, panels,N)%>%distinct(),
            aes(x = 0.2, y = 6, label = N)) +
  geom_text(data = dataFig%>%filter(models == "Model 4")%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.18, y = 6, label = sampleSize)) +
  geom_text(data = dataFig%>%filter(models != "Model 4")%>%select(models, panels,N)%>%distinct(),
            aes(x = 0.32, y = 6, label = N)) + 
  geom_text(data = dataFig%>%filter(models != "Model 4")%>%select(models, panels,sampleSize)%>%distinct(),
            aes(x = 0.28, y = 6, label = sampleSize)) + 
  facet_grid( ~ models, scales = "free") +
  xlab("Effect Size") +
  ylab("Frequency") + 
  theme_bw() + theme(legend.position = "none")

ggsave("Figure-SI11.pdf", histFig, width = 14, height = 6)


