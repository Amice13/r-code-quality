##################################################################################################
##################################################################################################
# This R-file generates all the tables and figures in the paper and SI 

##################################################################################################
# The first part generates tables and figures with
# units of analysis being country - election type - period

# install.packages("renv")
# renv::init()  # Initializes the environment and captures current package versions

## Restore the exact R environment that is needed to replicate our results
install.packages("renv")
renv::restore()

# Load libraries
library(tidyverse);library(modelsummary);library(texreg);library(fixest)
library(tinytable);library(gridExtra);library(panelView)

# Set seeds
set.seed(42)
# Set model output parameters
gm <- tibble::tribble(~raw,            ~clean,          ~fmt,
                      "nobs",          "Observations",   0,
                      "adj.r.squared", "Adj. R2",        2)
options("modelsummary_format_numeric_latex" = "plain")

#################################################
###########         Run Models        ###########
#################################################

# Read dataset
didData = read_csv("../Data/mainData_period.csv") %>% mutate(cowcode = factor(cowcode))
didData = panel(didData, ~ country_election + period_num)

# Set variables
# Independent variables
vecIV = c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2")
# Dependent variables
vecDV = c('other_irreg','pei_faircount',"registry_irreg","pei_delay",'govt_intimidation',"nelda15","nelda33",
          'v2xnp_client','elec_quality','fraud_monitored','nelda29','ecav_violence','pre_violence','post_violence',
          "freefair","opposition_prevented_nelda13","campaign_media",
          "domestic_monitors","inter_monitors","pei_laws","pei_procedures","incumbent_vote_share")
# Control variables
vecControls = c('l_le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'l_lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45')

# Set models
# Plain FE
did_str_FE = paste0("outcome ~ treatment + ", paste0(vecControls, collapse = " + "), " | factor(country_election)")
# Plain TWFE
did_str_TWFE = paste0("outcome ~ treatment + ", paste0(vecControls, collapse = " + "), " | factor(country_election) + factor(period_num)")
vecModels = c(did_str_FE, did_str_TWFE)

#####################################################################
# Get all the model specifications
model_specs_main = expand.grid(outcome = vecDV, treatment = vecIV, 
                               model_spec = vecModels,stringsAsFactors = FALSE)
model_specs_main = model_specs_main %>%
  mutate(model_spec_type = case_when(
    model_specs_main$model_spec==did_str_FE~"Simple FE",
    model_specs_main$model_spec==did_str_TWFE~"TWFE")) %>%
  mutate(treatment = factor(treatment, levels = vecIV, labels = vecIV)) %>%
  mutate(outcome = factor(outcome, levels = vecDV, labels = vecDV)) %>%
  arrange(outcome,treatment)

# Run all models
run_all_models = list()
for(i in 1:nrow(model_specs_main)){
  # select outcome
  outcome = model_specs_main$outcome[i]
  treatment = model_specs_main$treatment[i]
  model_spec = model_specs_main$model_spec[i]
  model_spec_type = model_specs_main$model_spec_type[i]
  
  modelData = didData
  modelData$outcome = didData %>% pull(outcome)
  modelData$treatment = didData %>% pull(treatment)
  
  model_out = feols(as.formula(model_spec),data = modelData,cluster = ~country_name)
  run_all_models[[i]] = model_out}

#####################################################################
# Run model for NELDA 24 variable separately 
# restrict sample to be only those observations with well-defined incumbents
presData = didData %>% filter(!is.na(incumbent_vote_share))

vecDV = c('nelda24')

model_specs_nelda24 = expand.grid(
  outcome = vecDV,
  treatment = vecIV,
  model_spec = vecModels,
  stringsAsFactors = FALSE)

model_specs_nelda24 = model_specs_nelda24 %>%
  mutate(model_spec_type = case_when(model_specs_nelda24$model_spec==did_str_FE~"Simple FE",model_specs_nelda24$model_spec==did_str_TWFE~"TWFE")) %>%
  mutate(treatment = factor(treatment, levels = vecIV, labels = vecIV)) %>%
  mutate(outcome = factor(outcome, levels = vecDV, labels = vecDV)) %>%
  arrange(outcome,treatment)

run_all_models_nelda24 = list()
for(i in 1:nrow(model_specs_nelda24)){
  ## select outcome
  outcome = model_specs_nelda24$outcome[i]
  treatment = model_specs_nelda24$treatment[i]
  model_spec = model_specs_nelda24$model_spec[i]
  model_spec_type = model_specs_nelda24$model_spec_type[i]
  
  modelData = presData
  modelData$outcome = presData %>% pull(outcome)
  modelData$treatment = presData %>% pull(treatment)
  
  model_out = feols(as.formula(model_spec),data = modelData,cluster = ~country_name)
  run_all_models_nelda24[[i]] = model_out}

# Set the main coefficient
cm = c("treatment" = "Granularity")


######################################################
###########          Paper Table 1         ###########
######################################################

descDataTable1 = didData %>% as.data.frame() %>% select(-nelda24) %>%
  left_join(didData %>% as.data.frame() %>% filter(!is.na(incumbent_vote_share)) %>% select(country_name, election_type, period_num, nelda24), by = c('country_name', 'election_type', 'period_num')) %>% 
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2",'other_irreg','pei_faircount',"pei_delay", 'elec_quality','fraud_monitored','nelda29', 'nelda15','le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45',"l_le_wb_pop","l_lgdp_c15","freefair","incumbent_vote_share", "nelda24",
           "govt_intimidation","opposition_prevented_nelda13","campaign_media","nelda33","v2xnp_client","domestic_monitors","inter_monitors","registry_irreg","pei_laws","pei_procedures","ecav_violence","pre_violence","post_violence")) %>%
  drop_na("other_irreg","log_nlevel", vecControls) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std','other_irreg','pei_faircount'))

vtable::st(descDataTable1, digits = 2,
           labels = c("Log(units)","Granularity (level)","Granularity (std)","Other voting irregularities (V-Dem)","Unfair count (PEI)"),
           summ = c('mean(x)',"median(x)",'sd(x)','min(x)','max(x)','notNA(x)'),
           summ.names = c('Mean',"Median",'St. Dev.','Min','Max',"N"),
           out='latex', file = "../Results/Tables/paper-table1.tex") 


######################################################
###########          Paper Table 2         ###########
######################################################
inds1 = which(model_specs_main$outcome %in% c("other_irreg") & 
                model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                model_specs_main$model_spec_type == "Simple FE")

inds3 = which(model_specs_main$outcome %in% c("pei_faircount") & 
                model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                model_specs_main$model_spec_type == "Simple FE")

inds = c(inds1, inds3)

model1 = didData %>% as.data.frame() %>% drop_na('other_irreg',"log_nlevel", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_level", vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_u_std", vecControls) 

model4 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"log_nlevel", vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_level", vecControls) 
model6 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_u_std", vecControls) 

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}


rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  group_tt(j=list("(units)"=2,"(level)"=3,"(std)"=4,"(units)"=5,"(level)"=6,"(std)"=7)) %>%
  group_tt(j=list("Log"=2,"Granularity"=3,"Granularity"=4,"Log"=5,"Granularity"=6,"Granularity"=7)) %>%
  group_tt(j=list("Outcome" = 1,"Other voting irregularities (V-Dem)" = 2:4, "Unfair count (PEI)" = 5:7)) %>%
  save_tt(output = "../Results/Tables/paper-table2.tex", overwrite = TRUE)

######################################################
###########          Paper Table 3         ###########
######################################################
inds_vs = which(model_specs_main$outcome %in% c("incumbent_vote_share") &
                  model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type == "Simple FE")
inds_nelda24 = which(model_specs_nelda24$outcome %in% c("nelda24") &
                       model_specs_nelda24$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                       model_specs_nelda24$model_spec_type == "Simple FE")

model1 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"log_nlevel", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_level", vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_u_std", vecControls) 
model4 = presData %>% as.data.frame() %>% drop_na('nelda24',"log_nlevel", vecControls) 
model5 = presData %>% as.data.frame() %>% drop_na('nelda24',"policy_down_level", vecControls) 
model6 = presData %>% as.data.frame() %>% drop_na('nelda24',"policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds_vs){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}
for (i in inds_nelda24){
  mod = run_all_models_nelda24[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))))


attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)


modelsummary(c(run_all_models[inds_vs],run_all_models_nelda24[inds_nelda24]),
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",coef_map = cm) %>%
  group_tt(j=list("(units)"=2,"(level)"=3,"(std)"=4,"(units)"=5,"(level)"=6,"(std)"=7)) %>%
  group_tt(j=list("Log"=2,"Granularity"=3,"Granularity"=4,"Log"=5,"Granularity"=6,"Granularity"=7)) %>%
  group_tt(j=list("Outcome" = 1,"Incumbent vote share" = 2:4, "Losing probability (NELDA)" = 5:7)) %>%
  save_tt(output = "../Results/Tables/paper-table3.tex", overwrite = TRUE)

######################################################
###########    Paper Table 4 Panel A       ###########
######################################################

## Panel A:
indsA = c()
for (out in c('nelda33',"v2xnp_client",'govt_intimidation', "opposition_prevented_nelda13", "campaign_media")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  indsA = c(indsA, inds1)
}

## Calculate effect size
model1 = didData %>% as.data.frame() %>% drop_na("nelda33",'log_nlevel', vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na("v2xnp_client",'log_nlevel', vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na("govt_intimidation",'log_nlevel', vecControls)
model7 = didData %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13",'log_nlevel', vecControls) 
model9 = didData %>% as.data.frame() %>% drop_na("campaign_media",'log_nlevel', vecControls)

model2 = didData %>% as.data.frame() %>% drop_na("nelda33", "policy_down_level",vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("v2xnp_client", "policy_down_level",vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("govt_intimidation","policy_down_level", vecControls)
model8 = didData %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13", "policy_down_level",vecControls)
model10 = didData %>% as.data.frame() %>% drop_na("campaign_media", "policy_down_level",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsA){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",  
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsA],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/paper-table4-panelA.tex", overwrite = TRUE)

######################################################
###########    Paper Table 4 Panel B       ###########
######################################################
## Panel B:
indsB = c()
for (out in c('domestic_monitors',"inter_monitors",'registry_irreg', "pei_laws", "pei_procedures")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  indsB = c(indsB, inds1)
}

## Calculate effect size
model1 = didData %>% as.data.frame() %>% drop_na("domestic_monitors",'log_nlevel', vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na("inter_monitors",'log_nlevel', vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na("registry_irreg",'log_nlevel', vecControls)
model7 = didData %>% as.data.frame() %>% drop_na("pei_laws",'log_nlevel', vecControls) 
model9 = didData %>% as.data.frame() %>% drop_na("pei_procedures",'log_nlevel', vecControls) 

model2 = didData %>% as.data.frame() %>% drop_na("domestic_monitors", "policy_down_level",vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("inter_monitors", "policy_down_level",vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("registry_irreg","policy_down_level", vecControls)
model8 = didData %>% as.data.frame() %>% drop_na("pei_laws", "policy_down_level",vecControls)
model10 = didData %>% as.data.frame() %>% drop_na("pei_procedures", "policy_down_level",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsB){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsB],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/paper-table4-panelB.tex", overwrite = TRUE)

######################################################
###########    Descriptive statistics      ###########
######################################################

##########################
####   SI-Table F6   #####
##########################

# This creates paper SI-Table F6 all descriptive statistics including:
# all dependent variables that appear in both the main paper and the appendix except for two core variables in table1
# all control variables
# Note that the order of the variables is not perserved.
descDataTableF6 = didData %>% as.data.frame() %>% select(-nelda24) %>%
  left_join(didData %>% as.data.frame() %>% filter(!is.na(incumbent_vote_share)) %>% select(country_name, election_type, period_num, nelda24), by = c('country_name', 'election_type', 'period_num')) %>% 
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2",'other_irreg','pei_faircount',"pei_delay", 'elec_quality','fraud_monitored','nelda29', 'nelda15','le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45',"l_le_wb_pop","l_lgdp_c15","freefair","incumbent_vote_share", "nelda24",
           "govt_intimidation","opposition_prevented_nelda13","campaign_media","nelda33","v2xnp_client","domestic_monitors","inter_monitors","registry_irreg","pei_laws","pei_procedures","ecav_violence","pre_violence","post_violence")) %>%
  drop_na("other_irreg","log_nlevel", vecControls) %>%
  select(-c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2",
            "l_le_wb_pop","l_lgdp_c15",'other_irreg','pei_faircount')) %>%
  mutate(le_wb_pop = le_wb_pop/1000,
         lgdp_c15 = lgdp_c15/1000000)

vtable::st(descDataTableF6, digits = 2,
           labels = c("Delay in announcement (PEI)","Election fraud (V-Dem)", "Monitors-reported fraud (NELDA)", "Riots and protests (NELDA)", "Intimidation to opposition (NELDA)","Total population (in thousands)", "EMB autonomy", "EMB capacity", "Polity score","GDP (Million)","Urbanization","Size of legislatures","International monitors","Free and Fair (Barometers)", "Incumbent Vote Share", "Losing probability (Nelda)",
                      "Intimidation to opposition (V-Dem)","Opposition not allowed (NELDA)","Free media (V-Dem)", "Severe violence, civilians (NELDA)", "Clientelism (V-Dem)", "Domestic monitors (V-Dem)", "International monitors (V-Dem)","Registry irregularities (V-Dem)","Electoral laws (PEI)","Electoral procedures (PEI)", "Election violence (ECAV)","Pre-election violence (ECAV)","Post-election violence (ECAV)"),
           summ = c('mean(x)',"median(x)",'sd(x)','min(x)','max(x)','notNA(x)'),
           summ.names = c('Mean',"Median",'St. Dev.','Min','Max',"N"),
           out='latex', file = "../Results/Tables/SI-TableF6.tex") 

#########################
####   SI-Table D5  #####
#########################

# descriptive statistics on granularity by periods
#### 1. Period 1
descPeriod1 = didData %>% as.data.frame() %>%
  filter(period_num == 1) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2",'other_irreg','pei_faircount',"registry_irreg","pei_delay", 'elec_quality','fraud_monitored','nelda29','govt_intimidation', 'nelda15', 'nelda33', 'v2xnp_client','le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45',"l_le_wb_pop","l_lgdp_c15"))  %>%
  drop_na("other_irreg","log_nlevel", vecControls) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2"))


vtable::st(descPeriod1, digits = 2,
           labels = c("Log(units)","Granularity (level)","Granularity (std)","Granularity (median)","Granularity (moving average)"),
           summ = c('mean(x)',"median(x)",'sd(x)','min(x)','max(x)','notNA(x)'),
           summ.names = c('Mean',"Median",'St. Dev.','Min','Max',"N"),
           out='latex', file = "../Results/Tables/SI-TableD5-period1.tex") 

#### 2. Period 2
descPeriod2 = didData %>% as.data.frame() %>%
  filter(period_num == 2) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2",'other_irreg','pei_faircount',"registry_irreg","pei_delay", 'elec_quality','fraud_monitored','nelda29','govt_intimidation', 'nelda15', 'nelda33', 'v2xnp_client','le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45',"l_le_wb_pop","l_lgdp_c15"))  %>%
  drop_na("other_irreg","log_nlevel", vecControls) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2"))


vtable::st(descPeriod2, digits = 2,
           labels = c("Log(units)","Granularity (level)","Granularity (std)","Granularity (median)","Granularity (moving average)"),
           summ = c('mean(x)',"median(x)",'sd(x)','min(x)','max(x)','notNA(x)'),
           summ.names = c('Mean',"Median",'St. Dev.','Min','Max',"N"),
           out='latex', file = "../Results/Tables/SI-TableD5-period2.tex") 


#### 3. Period 3
descPeriod3 = didData %>% as.data.frame() %>%
  filter(period_num == 3) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2",'other_irreg','pei_faircount',"registry_irreg","pei_delay", 'elec_quality','fraud_monitored','nelda29','govt_intimidation', 'nelda15', 'nelda33', 'v2xnp_client','le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45',"l_le_wb_pop","l_lgdp_c15"))  %>%
  drop_na("other_irreg","log_nlevel", vecControls) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2"))


vtable::st(descPeriod3, digits = 2,
           labels = c("Log(units)","Granularity (level)","Granularity (std)","Granularity (median)","Granularity (moving average)"),
           summ = c('mean(x)',"median(x)",'sd(x)','min(x)','max(x)','notNA(x)'),
           summ.names = c('Mean',"Median",'St. Dev.','Min','Max',"N"),
           out='latex', file = "../Results/Tables/SI-TableD5-period3.tex") 

#### 4. Period 4
descPeriod4 = didData %>% as.data.frame() %>%
  filter(period_num == 4) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2",'other_irreg','pei_faircount',"registry_irreg","pei_delay", 'elec_quality','fraud_monitored','nelda29','govt_intimidation', 'nelda15', 'nelda33', 'v2xnp_client','le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45',"l_le_wb_pop","l_lgdp_c15"))  %>%
  drop_na("other_irreg","log_nlevel", vecControls) %>%
  select(c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2"))


vtable::st(descPeriod4, digits = 2,
           labels = c("Log(units)","Granularity (level)","Granularity (std)","Granularity (median)","Granularity (moving average)"),
           summ = c('mean(x)',"median(x)",'sd(x)','min(x)','max(x)','notNA(x)'),
           summ.names = c('Mean',"Median",'St. Dev.','Min','Max',"N"),
           out='latex', file = "../Results/Tables/SI-TableD5-period4.tex") 

######################################################
###########          SI-Table D4           ###########
######################################################

corIV = didData %>% as.data.frame() %>% select(all_of(vecIV))
colnames(corIV) = c("Log(units)","Level", "Std", "Median", "Moving average")

as.data.frame(cor(corIV)) %>% round(3) %>% kableExtra::kbl(format="latex") %>% kableExtra::kable_classic() %>%
  kableExtra::save_kable("../Results/Tables/SI-tableD4.tex")

######################################################
###########          SI-Table H3           ###########
######################################################

ranking_pres1 = didData %>% as.data.frame() %>%
  filter(period_num==1, election_type=="Presidential") %>%
  select(country_name, unit_per_capita) %>%
  arrange(unit_per_capita) %>%
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period1 = unit_per_capita)

ranking_pres2 = didData %>% as.data.frame() %>%
  filter(period_num==2, election_type=="Presidential") %>%
  select(country_name, unit_per_capita) %>% 
  arrange(unit_per_capita) %>% 
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period2 = unit_per_capita)

ranking_pres3 = didData %>% as.data.frame() %>% 
  filter(period_num==3, election_type=="Presidential") %>%
  select(country_name, unit_per_capita) %>% 
  arrange(unit_per_capita) %>% 
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period3 = unit_per_capita)

ranking_pres4 = didData %>% as.data.frame() %>% 
  filter(period_num==4, election_type=="Presidential") %>%
  select(country_name, unit_per_capita) %>% 
  arrange(unit_per_capita) %>% 
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period4 = unit_per_capita)

ranking_pres = merge(merge(merge(ranking_pres1, ranking_pres2, by="Rank", all = T),
                           ranking_pres3, by="Rank", all = T),
                     ranking_pres4, by="Rank", all = T)
write.csv(ranking_pres, "../Results/Tables/SI-TableH3.csv", row.names = F, na = "")

######################################################
###########          SI-Table H4           ###########
######################################################

ranking_parl1 = didData %>% as.data.frame() %>% 
  filter(period_num==1, election_type=="Legislative") %>%
  select(country_name, unit_per_capita) %>%
  arrange(unit_per_capita) %>%
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period1 = unit_per_capita)

ranking_parl2 = didData %>% as.data.frame() %>% 
  filter(period_num==2, election_type=="Legislative") %>%
  select(country_name, unit_per_capita) %>% 
  arrange(unit_per_capita) %>% 
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period2 = unit_per_capita)

ranking_parl3 = didData %>% as.data.frame() %>% 
  filter(period_num==3, election_type=="Legislative") %>%
  select(country_name, unit_per_capita) %>% 
  arrange(unit_per_capita) %>% 
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period3 = unit_per_capita)

ranking_parl4 = didData %>% as.data.frame() %>% 
  filter(period_num==4, election_type=="Legislative") %>%
  select(country_name, unit_per_capita) %>% 
  arrange(unit_per_capita) %>% 
  mutate(unit_per_capita = unit_per_capita*100000,
         Rank = row_number()) %>%
  mutate(unit_per_capita = round(unit_per_capita,3)) %>%
  mutate(country_name = stringr::str_to_title(country_name)) %>%
  rename(Period4 = unit_per_capita)

ranking_parl = merge(merge(merge(ranking_parl1, ranking_parl2, by="Rank", all = T),
                           ranking_parl3, by="Rank", all = T),
                     ranking_parl4, by="Rank", all = T)

write.csv(ranking_parl, "../Results/Tables/SI-TableH4.csv", row.names = F, na = "")

######################################################
###########          SI-Table I5           ###########
######################################################
inds = which(model_specs_main$outcome %in% c("pei_delay") &
               model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std','policy_down_u_50p', 'policy_down_u_g2') &
               model_specs_main$model_spec_type == "Simple FE")

model1 = didData %>% as.data.frame() %>% drop_na('pei_delay',"log_nlevel", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('pei_delay',"policy_down_level", vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na('pei_delay',"policy_down_u_std", vecControls) 
model4 = didData %>% as.data.frame() %>% drop_na("pei_delay","policy_down_u_50p", vecControls)
model5 = didData %>% as.data.frame() %>% drop_na("pei_delay","policy_down_u_g2", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(pei_delay) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(pei_delay) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(pei_delay) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(pei_delay) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(pei_delay) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4], vecCE[5])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm)  %>%
  save_tt(output = "../Results/Tables/SI-TableI5.tex", overwrite = TRUE)

################################################
###########       SI-Table I6        ###########
################################################

## DV: Logged(unit)
model_determinants = fixest::feols(log_nlevel ~ lv2elembaut + lv2elembcap + l_le_wb_pop + 
                                     le_polity2 + l_lgdp_c15 + lurban + ltotalseats + lnelda45 | 
                                     factor(country_election), data = didData, cluster = ~cowcode)

coefficient_determinant = c("lv2elembaut"="EMB autonomy",
                            "lv2elembcap"="EMB capacity",
                            "l_le_wb_pop"="Logged population",
                            "l_lgdp_c15"="Logged GDP",
                            "lurban"="Urbanization",
                            "le_polity2" = "Polity",
                            "ltotalseats"="Size of legislatures",
                            "lnelda45" = "Intl. Monitor")

modelsummary(list(model_determinants),
             col.names = c("","(1)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             output = "latex",
             coef_map = coefficient_determinant) %>%
  save_tt(output = "../Results/Tables/SI-TableI6.tex", overwrite = TRUE)


######################################################
###########          SI-Table I7           ###########
######################################################
inds = which(model_specs_main$outcome %in% c("nelda15") &
               model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std','policy_down_u_50p', 'policy_down_u_g2') &
               model_specs_main$model_spec_type == "Simple FE")

model1 = didData %>% as.data.frame() %>% drop_na('nelda15',"log_nlevel", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('nelda15',"policy_down_level", vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na('nelda15',"policy_down_u_std", vecControls) 
model4 = didData %>% as.data.frame() %>% drop_na("nelda15","policy_down_u_50p", vecControls)
model5 = didData %>% as.data.frame() %>% drop_na("nelda15","policy_down_u_g2", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(nelda15) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(nelda15) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(nelda15) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(nelda15) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(nelda15) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4], vecCE[5])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableI7.tex", overwrite = TRUE)

######################################################
###########          SI-Table I8           ###########
######################################################
inds = c()
for (out in c('ecav_violence','pre_violence','post_violence')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  inds = c(inds, inds1)}

model1 = didData %>% as.data.frame() %>% drop_na("ecav_violence","log_nlevel", vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("pre_violence","log_nlevel", vecControls)
model7 = didData %>% as.data.frame() %>% drop_na("post_violence", "log_nlevel",vecControls)

model2 = didData %>% as.data.frame() %>% drop_na("ecav_violence","policy_down_level", vecControls)
model5 = didData %>% as.data.frame() %>% drop_na("pre_violence","policy_down_level", vecControls)
model8 = didData %>% as.data.frame() %>% drop_na("post_violence","policy_down_level", vecControls)

model3 = didData %>% as.data.frame() %>% drop_na("ecav_violence","policy_down_u_std", vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("pre_violence","policy_down_u_std",  vecControls)
model9 = didData %>% as.data.frame() %>% drop_na("post_violence", "policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}


rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",  ~"(7)",  ~"(8)",~"(9)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Treatment/Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(ecav_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(ecav_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(ecav_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(pre_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[5]/(model5%>% pull(pre_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[6]/(model6%>% pull(pre_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[7]/(model7%>% pull(post_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[8]/(model8%>% pull(post_violence) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[9]/(model9%>% pull(post_violence) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),as.character(length(unique(model7$country_name))),as.character(length(unique(model8$country_name))),as.character(length(unique(model9$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4], vecCE[5], vecCE[6],  vecCE[7], vecCE[8], vecCE[9])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableI8.tex", overwrite = TRUE)

####################################################
###########    SI-Table I9 Panel A       ###########
####################################################
inds = c()
for (out in c('elec_quality','fraud_monitored')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  inds = c(inds, inds1)}

model1 = didData %>% as.data.frame() %>% drop_na("elec_quality","log_nlevel", vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","log_nlevel", vecControls)
model2 = didData %>% as.data.frame() %>% drop_na("elec_quality","policy_down_level", vecControls)
model5 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_level", vecControls)
model3 = didData %>% as.data.frame() %>% drop_na("elec_quality","policy_down_u_std", vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[5]/(model5%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[6]/(model6%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableI9_panelA.tex", overwrite = TRUE)

####################################################
###########    SI-Table I9 Panel B       ###########
####################################################
inds = c()
for (out in c('freefair','nelda29')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  inds = c(inds, inds1)}

model1 = didData %>% as.data.frame() %>% drop_na("freefair","log_nlevel", vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("nelda29","log_nlevel", vecControls)
model2 = didData %>% as.data.frame() %>% drop_na("freefair","policy_down_level", vecControls)
model5 = didData %>% as.data.frame() %>% drop_na("nelda29","policy_down_level", vecControls)
model3 = didData %>% as.data.frame() %>% drop_na("freefair","policy_down_u_std", vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("nelda29","policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(nelda29) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[5]/(model5%>% pull(nelda29) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[6]/(model6%>% pull(nelda29) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableI9_panelB.tex", overwrite = TRUE)


############################################
###########    SI-Table J1       ###########
############################################
inds1 = which(model_specs_main$outcome %in% c("other_irreg") & 
                model_specs_main$treatment %in% c('policy_down_u_50p', 'policy_down_u_g2') &
                model_specs_main$model_spec_type == "Simple FE")

inds3 = which(model_specs_main$outcome %in% c("pei_faircount") & 
                model_specs_main$treatment %in% c('policy_down_u_50p', 'policy_down_u_g2') &
                model_specs_main$model_spec_type == "Simple FE")

inds = c(inds1, inds3)

model1 = didData %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_u_50p", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_u_g2", vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_u_50p", vecControls) 
model4 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_u_g2", vecControls) 

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ1.tex", overwrite = TRUE)

####################################################
###########    SI-Table J2 Panel A       ###########
####################################################
## Panel A:
indsA = c()
for (out in c('nelda33',"v2xnp_client",'govt_intimidation', "opposition_prevented_nelda13", "campaign_media")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('policy_down_u_std','policy_down_u_g2') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  indsA = c(indsA, inds1)}

## Calculate effect size
model1 = didData %>% as.data.frame() %>% drop_na("nelda33",'policy_down_u_std', vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na("v2xnp_client",'policy_down_u_std', vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na("govt_intimidation",'policy_down_u_std', vecControls)
model7 = didData %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13",'policy_down_u_std', vecControls) 
model9 = didData %>% as.data.frame() %>% drop_na("campaign_media",'policy_down_u_std', vecControls) 

model2 = didData %>% as.data.frame() %>% drop_na("nelda33", "policy_down_u_g2",vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("v2xnp_client", "policy_down_u_g2",vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("govt_intimidation","policy_down_u_g2", vecControls)
model8 = didData %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13", "policy_down_u_g2",vecControls)
model10 = didData %>% as.data.frame() %>% drop_na("campaign_media", "policy_down_u_g2",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsA){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",  
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsA],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ2-panelA.tex", overwrite = TRUE)

####################################################
###########    SI-Table J2 Panel B       ###########
####################################################
## Panel B:
indsB = c()
for (out in c('domestic_monitors',"inter_monitors",'registry_irreg', "pei_laws", "pei_procedures")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('policy_down_u_std','policy_down_u_g2') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  indsB = c(indsB, inds1)
}

## Calculate effect size
model1 = didData %>% as.data.frame() %>% drop_na("domestic_monitors",'policy_down_u_std', vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na("inter_monitors",'policy_down_u_std', vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na("registry_irreg",'policy_down_u_std', vecControls)
model7 = didData %>% as.data.frame() %>% drop_na("pei_laws",'policy_down_u_std', vecControls) 
model9 = didData %>% as.data.frame() %>% drop_na("pei_procedures",'policy_down_u_std', vecControls)

model2 = didData %>% as.data.frame() %>% drop_na("domestic_monitors", "policy_down_u_g2",vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("inter_monitors", "policy_down_u_g2",vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("registry_irreg","policy_down_u_g2", vecControls)
model8 = didData %>% as.data.frame() %>% drop_na("pei_laws", "policy_down_u_g2",vecControls)
model10 = didData %>% as.data.frame() %>% drop_na("pei_procedures", "policy_down_u_g2",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsB){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsB],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ2-panelB.tex", overwrite = TRUE)


####################################################
###########    SI-Table J3 Panel A       ###########
####################################################
inds = c()
for (out in c('elec_quality','fraud_monitored')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('policy_down_u_50p', 'policy_down_u_g2') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  inds = c(inds, inds1)}

model1 = didData %>% as.data.frame() %>% drop_na("elec_quality","policy_down_u_50p", vecControls)
model3 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_u_50p", vecControls)
model2 = didData %>% as.data.frame() %>% drop_na("elec_quality","policy_down_u_g2", vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_u_g2", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableJ3-panelA.tex", overwrite = TRUE)

####################################################
###########    SI-Table J3 Panel B       ###########
####################################################
inds = c()
for (out in c('freefair','nelda29')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('policy_down_u_50p', 'policy_down_u_g2') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  inds = c(inds, inds1)
}

model1 = didData %>% as.data.frame() %>% drop_na("freefair","policy_down_u_50p", vecControls)
model3 = didData %>% as.data.frame() %>% drop_na("nelda29","policy_down_u_50p", vecControls)
model2 = didData %>% as.data.frame() %>% drop_na("freefair","policy_down_u_g2", vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("nelda29","policy_down_u_g2", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}


rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(nelda29) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(nelda29) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableJ3-panelB.tex", overwrite = TRUE)

############################################
###########    SI-Table J4       ###########
############################################
inds_vs = which(model_specs_main$outcome %in% c("incumbent_vote_share") &
                  model_specs_main$treatment %in% c('policy_down_u_50p', 'policy_down_u_g2') &
                  model_specs_main$model_spec_type == "Simple FE")
inds_nelda24 = which(model_specs_nelda24$outcome %in% c("nelda24") &
                       model_specs_nelda24$treatment %in% c('policy_down_u_50p', 'policy_down_u_g2') &
                       model_specs_nelda24$model_spec_type == "Simple FE")

model1 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_u_50p", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_u_g2", vecControls) 
model3 = presData %>% as.data.frame() %>% drop_na('nelda24',"policy_down_u_50p", vecControls) 
model4 = presData %>% as.data.frame() %>% drop_na('nelda24',"policy_down_u_g2", vecControls) 

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds_vs){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}
for (i in inds_nelda24){
  mod = run_all_models_nelda24[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}


rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4])


attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(c(run_all_models[inds_vs],run_all_models_nelda24[inds_nelda24]),
             col.names = c("","(1)","(2)","(3)","(4)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ4.tex", overwrite = TRUE)

############################################
###########    SI-Table J5       ###########
############################################
inds1 = which(model_specs_main$outcome %in% c("other_irreg") & 
                model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                model_specs_main$model_spec_type == "TWFE")

inds3 = which(model_specs_main$outcome %in% c("pei_faircount") & 
                model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                model_specs_main$model_spec_type == "TWFE")

inds = c(inds1, inds3)

model1 = didData %>% as.data.frame() %>% drop_na('other_irreg',"log_nlevel", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_level", vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_u_std", vecControls) 

model4 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"log_nlevel", vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_level", vecControls) 
model6 = didData %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_u_std", vecControls) 

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}


rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm)  %>%
  save_tt(output = "../Results/Tables/SI-TableJ5.tex", overwrite = TRUE)

####################################################
###########    SI-Table J6 Panel A       ###########
####################################################

## Panel A:
indsA = c()
for (out in c('nelda33',"v2xnp_client", 'govt_intimidation', "opposition_prevented_nelda13", "campaign_media")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level') &
                  model_specs_main$model_spec_type %in% c("TWFE"))
  indsA = c(indsA, inds1)}

## Calculate effect size
model1 = didData %>% as.data.frame() %>% drop_na("nelda33",'log_nlevel', vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na("v2xnp_client",'log_nlevel', vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na("govt_intimidation",'log_nlevel', vecControls)
model7 = didData %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13",'log_nlevel', vecControls) 
model9 = didData %>% as.data.frame() %>% drop_na("campaign_media",'log_nlevel', vecControls)

model2 = didData %>% as.data.frame() %>% drop_na("nelda33", "policy_down_level",vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("v2xnp_client", "policy_down_level",vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("govt_intimidation","policy_down_level", vecControls)
model8 = didData %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13", "policy_down_level",vecControls)
model10 = didData %>% as.data.frame() %>% drop_na("campaign_media", "policy_down_level",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsA){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",  
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsA],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ6-panelA.tex", overwrite = TRUE)

####################################################
###########    SI-Table J6 Panel B       ###########
####################################################
## Panel B:
indsB = c()
for (out in c('domestic_monitors',"inter_monitors",'registry_irreg',"pei_laws", "pei_procedures")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level') &
                  model_specs_main$model_spec_type %in% c("TWFE"))
  indsB = c(indsB, inds1)
}

## Calculate effect size
model1 = didData %>% as.data.frame() %>% drop_na("domestic_monitors",'log_nlevel', vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na("inter_monitors",'log_nlevel', vecControls) 
model5 = didData %>% as.data.frame() %>% drop_na("registry_irreg",'log_nlevel', vecControls)
model7 = didData %>% as.data.frame() %>% drop_na("pei_laws",'log_nlevel', vecControls) 
model9 = didData %>% as.data.frame() %>% drop_na("pei_procedures",'log_nlevel', vecControls) 

model2 = didData %>% as.data.frame() %>% drop_na("domestic_monitors", "policy_down_level",vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("inter_monitors", "policy_down_level",vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("registry_irreg","policy_down_level", vecControls)
model8 = didData %>% as.data.frame() %>% drop_na("pei_laws", "policy_down_level",vecControls)
model10 = didData %>% as.data.frame() %>% drop_na("pei_procedures", "policy_down_level",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsB){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsB],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ6-panelB.tex", overwrite = TRUE)

####################################################
###########    SI-Table J7 Panel A       ###########
####################################################
inds = c()
for (out in c('elec_quality','fraud_monitored')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type %in% c("TWFE"))
  inds = c(inds, inds1)}

model1 = didData %>% as.data.frame() %>% drop_na("elec_quality","log_nlevel", vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","log_nlevel", vecControls)
model2 = didData %>% as.data.frame() %>% drop_na("elec_quality","policy_down_level", vecControls)
model5 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_level", vecControls)
model3 = didData %>% as.data.frame() %>% drop_na("elec_quality","policy_down_u_std", vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}


rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[5]/(model5%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[6]/(model6%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableJ7-panelA.tex", overwrite = TRUE)

####################################################
###########    SI-Table J7 Panel B       ###########
####################################################
inds = c()
for (out in c('freefair','nelda29')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type %in% c("TWFE"))
  inds = c(inds, inds1)}

model1 = didData %>% as.data.frame() %>% drop_na("freefair","log_nlevel", vecControls)
model4 = didData %>% as.data.frame() %>% drop_na("nelda29","log_nlevel", vecControls)
model2 = didData %>% as.data.frame() %>% drop_na("freefair","policy_down_level", vecControls)
model5 = didData %>% as.data.frame() %>% drop_na("nelda29","policy_down_level", vecControls)
model3 = didData %>% as.data.frame() %>% drop_na("freefair","policy_down_u_std", vecControls)
model6 = didData %>% as.data.frame() %>% drop_na("nelda29","policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(nelda29) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[5]/(model5%>% pull(nelda29) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[6]/(model6%>% pull(nelda29) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableJ7-panelB.tex", overwrite = TRUE)

############################################
###########    SI-Table J8       ###########
############################################
inds_vs = which(model_specs_main$outcome %in% c("incumbent_vote_share") &
                  model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type == "TWFE")
inds_nelda24 = which(model_specs_nelda24$outcome %in% c("nelda24") &
                       model_specs_nelda24$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                       model_specs_nelda24$model_spec_type == "TWFE")

model1 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"log_nlevel", vecControls) 
model2 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_level", vecControls) 
model3 = didData %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_u_std", vecControls) 
model4 = presData %>% as.data.frame() %>% drop_na('nelda24',"log_nlevel", vecControls) 
model5 = presData %>% as.data.frame() %>% drop_na('nelda24',"policy_down_level", vecControls) 
model6 = presData %>% as.data.frame() %>% drop_na('nelda24',"policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds_vs){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}
for (i in inds_nelda24){
  mod = run_all_models_nelda24[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4], vecCE[5], vecCE[6])


attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)


modelsummary(c(run_all_models[inds_vs],run_all_models_nelda24[inds_nelda24]),
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ8.tex", overwrite = TRUE)

#########################################################
###########    Paper Figure 1 Left Panel      ###########
#########################################################

boxContPeriod = didData %>% as.data.frame() %>%
  mutate(period_char = case_when(period_char == "period0005" ~ "2000 ~ 2004",
                                 period_char == "period0509" ~ "2005 ~ 2009",
                                 period_char == "period1015" ~ "2010 ~ 2014",
                                 period_char == "period1520" ~ "2015 ~ 2020")) %>%
  ggplot(aes(x = period_char, y=log_unit_per_capita, color = period_char)) +
  geom_boxplot() +
  scale_color_manual(values = c("black","plum4","darkblue","lightblue")) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, show.legend = FALSE) +
  labs(x = "", y = "Log (units per capita)",color = "Period") + 
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 8),
        legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_blank())

pdf("../Results/Figures/paper-Figure1-LeftPanel.pdf", width = 8, height = 5)
print(boxContPeriod)
dev.off()

#########################################################
###########    Paper Figure 1 Right Panel      ##########
#########################################################
treatment_status = didData %>% as.data.frame() %>% mutate(country_election = paste(country_name, election_type)) %>% 
  mutate(year_temp = case_when(period_num == 1 ~ 2005,
                               period_num == 2 ~ 2010,
                               period_num == 3 ~ 2015,
                               period_num == 4 ~ 2020))

level = panelview(other_irreg ~ policy_down_level, data = treatment_status, index = c("country_election","period_num"), 
                  xlab = "Period", ylab = "", axis.lab = "time", main = "Country election Granularity (level)",
                  legend.labs = c("Granularity = 0", "Granularity = 1", "Missing"),
                  color = c("grey70","black","white"),gridOff = TRUE,
                  background = "white",cex.legend = 8,
                  cex.main = 12, cex.axis = 8)

pdf("../Results/Figures/paper-Figure1-RightPanel.pdf", width = 6, height = 4)
print(level)
dev.off()

#############################################
###########    Paper Figure 2      ##########
#############################################

scatterData = didData %>% as.data.frame() %>% drop_na(other_irreg, log_nlevel, l_le_wb_pop)
mod1 = lm(other_irreg ~ l_le_wb_pop, data = scatterData)
mod2 = lm(log_nlevel ~ l_le_wb_pop, data = scatterData)

scatter_raw = ggplot() +
  geom_point(aes(x = mod2$residuals, y = mod1$residuals)) +
  geom_smooth(aes(x = mod2$residuals, y = mod1$residuals), method="lm", color = "palegreen3") +
  geom_smooth(aes(x = mod2$residuals, y = mod1$residuals), method="loess", color = "plum4", se = F) +
  labs(x = "Log (units)", y = "Other voting irregularities (V-Dem)") + 
  theme_classic()

pdf("../Results/Figures/paper-Figure2.pdf", width = 8, height = 5)
print(scatter_raw)
dev.off()

###########################################
###########    SI-Figure D1      ##########
###########################################
std = panelview(other_irreg ~ policy_down_u_std, data = treatment_status, index = c("country_election","period_num"), 
                xlab = "Period", ylab = "", axis.lab = "time",
                color = c("grey70","black","white"),main = "Country election Granularity (std)",
                background = "white",cex.legend = 8,gridOff = TRUE,
                cex.main = 12, cex.axis = 8, legend.labs = c("Granularity = 0", "Granularity = 1", "Missing"))

pdf("../Results/Figures/SI-FigureD1.pdf", width = 10, height = 4, onefile = TRUE)
grid.arrange(level, std, nrow=1,ncol=2)
dev.off()

###########################################
###########    SI-Figure H1      ##########
###########################################
examples = didData %>% as.data.frame() %>%
  filter(country_name %in% c("argentina","brazil","georgia",
                             "paraguay","sri lanka","ecuador",
                             "namibia","ghana","bolivia")) %>%
  mutate(country_name = str_to_title(country_name)) %>%
  mutate(country_name = factor(country_name,
                               levels=c("Argentina","Brazil","Georgia",
                                        "Paraguay","Sri Lanka","Ecuador",
                                        "Namibia","Ghana","Bolivia"))) %>%
  ggplot(aes(x=period_num, y=log_unit_per_capita, color=election_type)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.5, show.legend = T, size = 0.3) + 
  facet_wrap(~country_name, scales = "free_y", ncol = 3, nrow = 3) +
  scale_color_manual(values = c("palegreen3","plum4")) +
  labs(x = "Period", y = "Log (units per capita)",color = "Election Type") + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10),
        legend.position = "bottom", legend.direction = "horizontal")

pdf("../Results/Figures/SI-FigureH1.pdf", width = 8, height = 5)
examples
dev.off()

###########################################
###########    SI-Figure I2      ##########
###########################################
vdem_full = read.csv("../Data/vdem.csv") %>% 
  select(country_name, year, v2elirreg) %>% mutate(other_irreg= -v2elirreg)

## calculate the percentage of countries that are reported fraud
vdem_perc = vdem_full %>% select(country_name, year, other_irreg) %>%
  group_by(year) %>%
  summarize(perc = mean(other_irreg, na.rm=T))

## scatter plot of raw data
otherIrreg_scatter = ggplot(vdem_full) +
  geom_point(aes(x=year, y=other_irreg)) +
  geom_smooth(aes(x=year, y=other_irreg), method="lm", se = F) +
  xlab("") +
  ylab("Other voting irregularities") +
  theme_classic() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

## line plot of percentage of countreis reported fraud over years
otherIrreg_perc = ggplot(vdem_perc) +
  geom_point(aes(x=year, y=perc)) +
  geom_smooth(aes(x=year, y=perc), method="lm", se = F, color = "palegreen4", alpha = 0.5) +
  geom_smooth(aes(x=year, y=perc), method="loess", se = F, color = "plum4", alpha = 0.5) +
  xlab("") +
  ylab("Average other voting irregularities score") +
  theme_classic() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

pdf("../Results/Figures/SI-FigureI2.pdf", width = 10, height = 4, onefile = TRUE)
grid.arrange(otherIrreg_scatter, otherIrreg_perc, nrow=1,ncol=2)
dev.off()

###########################################
###########    SI-Figure I3      ##########
###########################################
vdem_full = read.csv("../Data/vdem.csv") %>%
  select(country_name, year, v2elwestmon)

## calculate the percentage of countries that are reported fraud
vdem_perc = vdem_full %>% select(country_name, year, v2elwestmon) %>%
  group_by(year) %>%
  summarize(perc = mean(v2elwestmon, na.rm=T))

## scatter plot of raw data
vdemFraud_scatter = ggplot(vdem_full) +
  geom_point(aes(x=year, y=v2elwestmon)) +
  geom_smooth(aes(x=year, y=v2elwestmon), method="lm", se = F) +
  xlab("") +
  ylab("Monitors-reported fraud (0 = No, 1 = Yes)") +
  theme_classic() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

## line plot of percentage of countreis reported fraud over years
vdemFraud_perc = ggplot(vdem_perc) +
  geom_point(aes(x=year, y=perc)) +
  geom_smooth(aes(x=year, y=perc), method="lm", se = F, color = "palegreen4", alpha = 0.5) +
  geom_smooth(aes(x=year, y=perc), method="loess", se = F, color = "plum4", alpha = 0.5) +
  xlab("") +
  ylab("Percentage of countries\n with monitors-reported fraud") +
  theme_classic() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

pdf("../Results/Figures/SI-FigureI3.pdf", width = 10, height = 4, onefile = TRUE)
grid.arrange(vdemFraud_scatter, vdemFraud_perc, nrow=1,ncol=2)
dev.off()

###########################################
###########    SI-Figure I4      ##########
###########################################
vdem_full = read.csv("../Data/vdem.csv") %>% select(country_name, year, v2elfrfair) %>%
  mutate(elec_quality = -v2elfrfair)

## calculate the percentage of countries that are reported fraud
vdem_perc = vdem_full %>% select(country_name, year, elec_quality) %>%
  group_by(year) %>%
  summarize(perc = mean(elec_quality, na.rm=T))

## scatter plot of raw data
elecQuality_scatter = ggplot(vdem_full) +
  geom_point(aes(x=year, y=elec_quality)) +
  geom_smooth(aes(x=year, y=elec_quality), method="lm", se = F) +
  xlab("") +
  ylab("Election fraud") +
  theme_classic() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

## line plot of percentage of countreis reported fraud over years
elecQuality_perc = ggplot(vdem_perc) +
  geom_point(aes(x=year, y=perc)) +
  geom_smooth(aes(x=year, y=perc), method="lm", se = F, color = "palegreen4", alpha = 0.5) +
  geom_smooth(aes(x=year, y=perc), method="loess", se = F, color = "plum4", alpha = 0.5) +
  xlab("") +
  ylab("Average election fraud score") +
  theme_classic() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

pdf("../Results/Figures/SI-FigureI4.pdf", width = 10, height = 4, onefile = TRUE)
grid.arrange(elecQuality_scatter, elecQuality_perc, nrow=1,ncol=2)
dev.off()


##################################################################################################
##################################################################################################
# The following part generates all the tables in the paper and SI with 
# units of analysis being country - election type - election year

#################################################
###########         Run Models        ###########
#################################################

# Read dataset
didData_year = read_csv("../Data/mainData_yearly.csv") %>% mutate(cowcode = factor(cowcode))
didData_year = panel(didData_year, ~ country_election + year)

# Set variables
# Independent variables
vecIV = c('log_nlevel','policy_down_level', 'policy_down_u_std', 'policy_down_u_50p',"policy_down_u_g2")
# Dependent variables
vecDV = c('other_irreg','pei_faircount',"registry_irreg","pei_delay",'govt_intimidation',"nelda15","nelda33",
          'v2xnp_client','elec_quality','fraud_monitored','nelda29',
          "freefair","opposition_prevented_nelda13","campaign_media",
          "domestic_monitors","inter_monitors","pei_laws","pei_procedures","incumbent_vote_share")
# Control variables
vecControls = c('l_le_wb_pop', 'lv2elembaut', 'lv2elembcap', 'le_polity2', 'l_lgdp_c15', 'lurban', 'ltotalseats', 'lnelda45')

# Set models
## Plain FE
did_str_FE = paste0("outcome ~ treatment + ", paste0(vecControls, collapse = " + "), " | factor(country_election)")
## Plain TWFE
did_str_TWFE = paste0("outcome ~ treatment + ", paste0(vecControls, collapse = " + "), " | factor(country_election) + factor(year)")
vecModels = c(did_str_FE, did_str_TWFE)

#####################################################################
# Get all the model specifications
model_specs_main = expand.grid(outcome = vecDV, treatment = vecIV, 
                               model_spec = vecModels,stringsAsFactors = FALSE)
model_specs_main = model_specs_main %>%
  mutate(model_spec_type = case_when(
    model_specs_main$model_spec==did_str_FE~"Simple FE",
    model_specs_main$model_spec==did_str_TWFE~"TWFE")) %>%
  mutate(treatment = factor(treatment, levels = vecIV, labels = vecIV)) %>%
  mutate(outcome = factor(outcome, levels = vecDV, labels = vecDV)) %>%
  arrange(outcome,treatment)

# Run all models
run_all_models = list()
for(i in 1:nrow(model_specs_main)){
  ## select outcome
  outcome = model_specs_main$outcome[i]
  treatment = model_specs_main$treatment[i]
  model_spec = model_specs_main$model_spec[i]
  model_spec_type = model_specs_main$model_spec_type[i]
  
  modelData = didData_year
  modelData$outcome = didData_year %>% pull(outcome)
  modelData$treatment = didData_year %>% pull(treatment) 
  
  model_out = feols(as.formula(model_spec), data = modelData,cluster = ~country_name)
  run_all_models[[i]] = model_out}

cm = c("treatment" = "Granularity")

#####################################################################
# Run model for NELDA 24 variable separately 
# restrict sample to be only those observations with well-defined incumbents
presData_year = didData_year %>% filter(!is.na(incumbent_vote_share))

vecDV = c('nelda24')

model_specs_nelda24 = expand.grid(
  outcome = vecDV,
  treatment = vecIV,
  model_spec = vecModels,
  stringsAsFactors = FALSE)

model_specs_nelda24 = model_specs_nelda24 %>%
  mutate(model_spec_type = case_when(model_specs_nelda24$model_spec==did_str_FE~"Simple FE",model_specs_nelda24$model_spec==did_str_TWFE~"TWFE")) %>%
  mutate(treatment = factor(treatment, levels = vecIV, labels = vecIV)) %>%
  mutate(outcome = factor(outcome, levels = vecDV, labels = vecDV)) %>%
  arrange(outcome,treatment)

run_all_models_nelda24 = list()
for(i in 1:nrow(model_specs_nelda24)){
  ## select outcome
  outcome = model_specs_nelda24$outcome[i]
  treatment = model_specs_nelda24$treatment[i]
  model_spec = model_specs_nelda24$model_spec[i]
  model_spec_type = model_specs_nelda24$model_spec_type[i]
  
  modelData = presData_year
  modelData$outcome = presData_year %>% pull(outcome)
  modelData$treatment = presData_year %>% pull(treatment)
  
  model_out = feols(as.formula(model_spec),data = modelData,cluster = ~country_name)
  run_all_models_nelda24[[i]] = model_out}

######################################################
###########          SI-Table H1           ###########
######################################################

## Get the country-election list then manually check each entry
presList = didData_year %>% filter(policy_down_level == 1, election_type=="Presidential") %>% pull(country_name) %>% unique()

presData = didData_year %>% as.data.frame() %>%
  filter(election_type=="Presidential", country_name %in% presList) %>%
  select(country_name, year, election_type, unit_level, name_english_, nlevel,policy_down_level) %>%
  group_by(country_name) %>%
  mutate(lag.unit_level = dplyr::lag(unit_level, n = 1, default = NA),
         lag.name_english_ = dplyr::lag(name_english_, n = 1, default = NA),
         lag.nlevel = dplyr::lag(nlevel, n = 1, default = NA)) %>%
  ungroup() %>% filter(policy_down_level == 1, unit_level != lag.unit_level) %>%
  mutate(change_units = (nlevel - lag.nlevel)/lag.nlevel) %>%
  select(country_name,year,election_type, lag.name_english_,name_english_,
         lag.unit_level,unit_level,lag.nlevel,nlevel,change_units) %>%
  rename(`Unit Name Before` = lag.name_english_, `Unit Name After` = name_english_, 
         `Unit Level Before` = lag.unit_level, `Unit Level After` = unit_level,
         `Units Before` = lag.nlevel, `Units After` = nlevel,
         `Country Name` = country_name, Year = year, `Election Type` = election_type,
         `Change in Units (%)` = change_units)

presData |>
  select(-c(`Unit Level Before`,`Unit Level After`)) |>
  mutate(`Change in Units (%)` = round(`Change in Units (%)`, 2)) |> 
  mutate(`Unit Name Before` = stringr::str_to_title(`Unit Name Before`),
         `Unit Name After` = stringr::str_to_title(`Unit Name After`),
         `Country Name` = stringr::str_to_title(`Country Name`)) |>
  write_csv(file = "../Results/Tables/SI-TableH1.csv")

######################################################
###########          SI-Table H2           ###########
######################################################
parlList = didData_year %>% filter(policy_down_level == 1, election_type=="Legislative") %>% pull(country_name) %>% unique()

parlData = didData_year %>% as.data.frame() %>%
  filter(election_type=="Legislative", country_name %in% parlList) %>%
  select(country_name, year, election_type, unit_level, name_english_, nlevel,policy_down_level) %>%
  group_by(country_name) %>%
  mutate(lag.unit_level = dplyr::lag(unit_level, n = 1, default = NA),
         lag.name_english_ = dplyr::lag(name_english_, n = 1, default = NA),
         lag.nlevel = dplyr::lag(nlevel, n = 1, default = NA)) %>%
  ungroup() %>% filter(policy_down_level == 1, unit_level != lag.unit_level) %>%
  mutate(change_units = (nlevel - lag.nlevel)/lag.nlevel) %>%
  filter(change_units>=0) %>%
  select(country_name,year,election_type, lag.name_english_,name_english_,
         lag.unit_level,unit_level,lag.nlevel,nlevel,change_units) %>%
  rename(`Unit Name Before` = lag.name_english_, `Unit Name After` = name_english_, 
         `Unit Level Before` = lag.unit_level, `Unit Level After` = unit_level,
         `Units Before` = lag.nlevel, `Units After` = nlevel,
         `Country Name` = country_name, Year = year, `Election Type` = election_type,
         `Change in Units (%)` = change_units)

parlData |>
  select(-c(`Unit Level Before`,`Unit Level After`)) |>
  mutate(`Change in Units (%)` = round(`Change in Units (%)`, 2)) |> 
  mutate(`Unit Name Before` = stringr::str_to_title(`Unit Name Before`),
         `Unit Name After` = stringr::str_to_title(`Unit Name After`),
         `Country Name` = stringr::str_to_title(`Country Name`)) |>
  write_csv(file = "../Results/Tables/SI-TableH2.csv")

####################################################
###########          SI-Table J9         ###########
####################################################
inds1 = which(model_specs_main$outcome %in% c("other_irreg") & 
                model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                model_specs_main$model_spec_type == "Simple FE")

inds3 = which(model_specs_main$outcome %in% c("pei_faircount") & 
                model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                model_specs_main$model_spec_type == "Simple FE")

inds = c(inds1, inds3)

model1 = didData_year %>% as.data.frame() %>% drop_na('other_irreg',"log_nlevel", vecControls) 
model2 = didData_year %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_level", vecControls) 
model3 = didData_year %>% as.data.frame() %>% drop_na('other_irreg',"policy_down_u_std", vecControls) 

model4 = didData_year %>% as.data.frame() %>% drop_na('pei_faircount',"log_nlevel", vecControls) 
model5 = didData_year %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_level", vecControls) 
model6 = didData_year %>% as.data.frame() %>% drop_na('pei_faircount',"policy_down_u_std", vecControls) 

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(other_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(pei_faircount) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ9.tex", overwrite = TRUE)

####################################################
###########    SI-Table J10 Panel A      ###########
####################################################
## Panel A:
indsA = c()
for (out in c('nelda33',"v2xnp_client",'govt_intimidation', "opposition_prevented_nelda13", "campaign_media")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  indsA = c(indsA, inds1)}

## Calculate effect size
model1 = didData_year %>% as.data.frame() %>% drop_na("nelda33",'log_nlevel', vecControls) 
model3 = didData_year %>% as.data.frame() %>% drop_na("v2xnp_client",'log_nlevel', vecControls) 
model5 = didData_year %>% as.data.frame() %>% drop_na("govt_intimidation",'log_nlevel', vecControls)
model7 = didData_year %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13",'log_nlevel', vecControls) 
model9 = didData_year %>% as.data.frame() %>% drop_na("campaign_media",'log_nlevel', vecControls) 

model2 = didData_year %>% as.data.frame() %>% drop_na("nelda33", "policy_down_level",vecControls)
model4 = didData_year %>% as.data.frame() %>% drop_na("v2xnp_client", "policy_down_level",vecControls)
model6 = didData_year %>% as.data.frame() %>% drop_na("govt_intimidation","policy_down_level", vecControls)
model8 = didData_year %>% as.data.frame() %>% drop_na("opposition_prevented_nelda13", "policy_down_level",vecControls)
model10 = didData_year %>% as.data.frame() %>% drop_na("campaign_media", "policy_down_level",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsA){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",  
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(nelda33) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(v2xnp_client) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(govt_intimidation) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(opposition_prevented_nelda13) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(campaign_media) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsA],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ10-panelA.tex", overwrite = TRUE)

####################################################
###########    SI-Table J10 Panel B      ###########
####################################################
## Panel B:
indsB = c()
for (out in c('domestic_monitors',"inter_monitors",'registry_irreg', "pei_laws", "pei_procedures")){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  indsB = c(indsB, inds1)
}

## Calculate effect size
model1 = didData_year %>% as.data.frame() %>% drop_na("domestic_monitors",'log_nlevel', vecControls) 
model3 = didData_year %>% as.data.frame() %>% drop_na("inter_monitors",'log_nlevel', vecControls) 
model5 = didData_year %>% as.data.frame() %>% drop_na("registry_irreg",'log_nlevel', vecControls)
model7 = didData_year %>% as.data.frame() %>% drop_na("pei_laws",'log_nlevel', vecControls) 
model9 = didData_year %>% as.data.frame() %>% drop_na("pei_procedures",'log_nlevel', vecControls) 

model2 = didData_year %>% as.data.frame() %>% drop_na("domestic_monitors", "policy_down_level",vecControls)
model4 = didData_year %>% as.data.frame() %>% drop_na("inter_monitors", "policy_down_level",vecControls)
model6 = didData_year %>% as.data.frame() %>% drop_na("registry_irreg","policy_down_level", vecControls)
model8 = didData_year %>% as.data.frame() %>% drop_na("pei_laws", "policy_down_level",vecControls)
model10 = didData_year %>% as.data.frame() %>% drop_na("pei_procedures", "policy_down_level",vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in indsB){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))
}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", ~"(7)",  ~"(8)",  ~"(9)",  ~"(10)",
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", 
                        as.character(round(vecCoefs[1]/(model1%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(domestic_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(inter_monitors) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(registry_irreg) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[7]/(model7%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[8]/(model8%>% pull(pei_laws) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[9]/(model9%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[10]/(model10%>% pull(pei_procedures) %>% mean() %>% abs()),2)),
                        "No. Countries",
                        as.character(length(unique(model1$country_name))),
                        as.character(length(unique(model2$country_name))),
                        as.character(length(unique(model3$country_name))),
                        as.character(length(unique(model4$country_name))),
                        as.character(length(unique(model5$country_name))),
                        as.character(length(unique(model6$country_name))),
                        as.character(length(unique(model7$country_name))),
                        as.character(length(unique(model8$country_name))),
                        as.character(length(unique(model9$country_name))),
                        as.character(length(unique(model10$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3], vecCE[4], vecCE[5], vecCE[6], vecCE[7],vecCE[8], vecCE[9], vecCE[10])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[indsB],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ10-panelB.tex", overwrite = TRUE)


####################################################
###########    SI-Table J11 Panel A      ###########
####################################################
inds = c()
for (out in c('elec_quality','fraud_monitored')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  inds = c(inds, inds1)}

model1 = didData_year %>% as.data.frame() %>% drop_na("elec_quality","log_nlevel", vecControls)
model4 = didData_year %>% as.data.frame() %>% drop_na("fraud_monitored","log_nlevel", vecControls)
model2 = didData_year %>% as.data.frame() %>% drop_na("elec_quality","policy_down_level", vecControls)
model5 = didData_year %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_level", vecControls)
model3 = didData_year %>% as.data.frame() %>% drop_na("elec_quality","policy_down_u_std", vecControls)
model6 = didData_year %>% as.data.frame() %>% drop_na("fraud_monitored","policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(elec_quality) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[5]/(model5%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[6]/(model6%>% pull(fraud_monitored) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableJ11-panelA.tex", overwrite = TRUE)

####################################################
###########    SI-Table J11 Panel B      ###########
####################################################
inds = c()
for (out in c('freefair','nelda29')){
  inds1 = which(model_specs_main$outcome == out & 
                  model_specs_main$treatment %in% c('log_nlevel','policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type %in% c("Simple FE"))
  inds = c(inds, inds1)}

model1 = didData_year %>% as.data.frame() %>% drop_na("freefair","log_nlevel", vecControls)
model4 = didData_year %>% as.data.frame() %>% drop_na("nelda29","log_nlevel", vecControls)
model2 = didData_year %>% as.data.frame() %>% drop_na("freefair","policy_down_level", vecControls)
model5 = didData_year %>% as.data.frame() %>% drop_na("nelda29","policy_down_level", vecControls)
model3 = didData_year %>% as.data.frame() %>% drop_na("freefair","policy_down_u_std", vecControls)
model6 = didData_year %>% as.data.frame() %>% drop_na("nelda29","policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
for (i in inds){
  mod = run_all_models[[i]]
  estimate = tidy(mod)%>%filter(term %in% c("treatment","stacktreat"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean",
                        as.character(round(vecCoefs[1]/(model1%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[2]/(model2%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[3]/(model3%>% pull(freefair) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[4]/(model4%>% pull(nelda29) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[5]/(model5%>% pull(nelda29) %>% mean() %>% abs()),2)),as.character(round(vecCoefs[6]/(model6%>% pull(nelda29) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1], vecCE[2], vecCE[3],  vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(run_all_models[inds],
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",
             coef_map = cm,
             notes = "") %>%
  save_tt(output = "../Results/Tables/SI-TableJ11-panelB.tex", overwrite = TRUE)

############################################
###########    SI-Table J12      ###########
############################################
inds_vs = which(model_specs_main$outcome %in% c("incumbent_vote_share") &
                  model_specs_main$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                  model_specs_main$model_spec_type == "Simple FE")
inds_nelda24 = which(model_specs_nelda24$outcome %in% c("nelda24") &
                       model_specs_nelda24$treatment %in% c('log_nlevel', 'policy_down_level', 'policy_down_u_std') &
                       model_specs_nelda24$model_spec_type == "Simple FE")

model1 = didData_year %>% as.data.frame() %>% drop_na('incumbent_vote_share',"log_nlevel", vecControls) 
model2 = didData_year %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_level", vecControls) 
model3 = didData_year %>% as.data.frame() %>% drop_na('incumbent_vote_share',"policy_down_u_std", vecControls) 
model4 = presData_year %>% as.data.frame() %>% drop_na('nelda24',"log_nlevel", vecControls) 
model5 = presData_year %>% as.data.frame() %>% drop_na('nelda24',"policy_down_level", vecControls) 
model6 = presData_year %>% as.data.frame() %>% drop_na('nelda24',"policy_down_u_std", vecControls)

vecCoefs = c()
vecCE = c() ## How many country-election
vecPeriod = c() ## How many period
for (i in inds_vs){
  mod = run_all_models[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}
for (i in inds_nelda24){
  mod = run_all_models_nelda24[[i]]
  estimate = broom::tidy(mod)%>%filter(term %in% c("treatment"))%>%pull(estimate)
  vecCoefs = c(vecCoefs, abs(estimate[1]))
  vecCE = c(vecCE, as.character(round(mod$fixef_sizes[1],0)))}

rows <- tibble::tribble(~term,   ~"(1)",  ~"(2)",  ~"(3)",  ~"(4)",  ~"(5)",  ~"(6)", 
                        "Country-Election FE", "Yes","Yes","Yes","Yes","Yes","Yes",
                        "Granularity Est./Mean", as.character(round(vecCoefs[1]/(model1%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[2]/(model2%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[3]/(model3%>% pull(incumbent_vote_share) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[4]/(model4%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[5]/(model5%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        as.character(round(vecCoefs[6]/(model6%>% pull(nelda24) %>% mean() %>% abs()),2)),
                        "No. Countries",as.character(length(unique(model1$country_name))),as.character(length(unique(model2$country_name))),as.character(length(unique(model3$country_name))),as.character(length(unique(model4$country_name))),as.character(length(unique(model5$country_name))),as.character(length(unique(model6$country_name))),
                        "No. Country-Elections", vecCE[1],vecCE[2], vecCE[3],   vecCE[4], vecCE[5], vecCE[6])

attr(rows, 'position') <- c(3, 4, 5, 6, 7, 8)

modelsummary(c(run_all_models[inds_vs],run_all_models_nelda24[inds_nelda24]),
             col.names = c("","(1)","(2)","(3)","(4)","(5)","(6)"),
             fmt = "%.3f",
             estimate = "{estimate}{stars}",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = gm,
             add_rows = rows,
             output = "latex",coef_map = cm) %>%
  save_tt(output = "../Results/Tables/SI-TableJ12.tex", overwrite = TRUE)


###########################################
###########    SI-Figure D2      ##########
###########################################


p1 = didData_year %>% as.data.frame() %>% 
  filter(election_type=="Presidential", name_english_ == "polling station") |> 
  ggplot() +
  geom_density(aes(x=nlevel)) +
  labs(title = "Presidential") +
  xlab("") + 
  theme_classic()

p2 = didData_year %>% as.data.frame() %>% 
  filter(election_type=="Legislative", name_english_ == "polling station") |> 
  ggplot() +
  geom_density(aes(x=nlevel)) +
  labs(title = "Legislative") +
  xlab("") + 
  theme_classic()
p3 = didData_year %>% as.data.frame() %>% 
  filter(name_english_ == "polling station") |> 
  ggplot() +
  geom_density(aes(x=nlevel)) +
  labs(title = "All Elections") +
  xlab("") + 
  theme_classic()

g1 = arrangeGrob(p1, p2, p3, ncol=3)
ggsave(file = "../Results/Figures/SI-FigureD2.pdf", g1,
       dpi = 800, width = 8, height = 3)


###########################################
###########    SI-Figure D3      ##########
###########################################

p1 = didData_year %>% as.data.frame() %>% 
  filter(election_type=="Presidential", unit_level == 1) |> 
  ggplot() +
  geom_density(aes(x=nlevel)) +
  labs(title = "Presidential") +
  xlab("") + 
  theme_classic()

p2 = didData_year %>% as.data.frame() %>% 
  filter(election_type=="Legislative", unit_level == 1) %>%
  filter(country_name != "egypt") |> 
  ggplot() +
  geom_density(aes(x=nlevel)) +
  labs(title = "Legislative") +
  xlab("") + 
  theme_classic()
p3 = didData_year %>% as.data.frame() %>% 
  filter(unit_level == 1) %>%
  filter(country_name != "egypt") |> 
  ggplot() +
  geom_density(aes(x=nlevel)) +
  labs(title = "All Elections") +
  xlab("") + 
  theme_classic()

g2 = arrangeGrob(p1, p2, p3, ncol=3)
ggsave(file = "../Results/Figures/SI-FigureD3.pdf", g2,
       dpi = 800, width = 8, height = 3)
