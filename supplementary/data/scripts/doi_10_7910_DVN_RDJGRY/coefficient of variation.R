# Open packages 

source('scripts/cleaning_and_management/libraries.R') 

# Code for split violin plots and adjusted labels

source('scripts/cleaning_and_management/split_violin_and_labels.R') 

# Load data 

# Delphi survey
expanel <- readRDS("data/Delphi survey/expanel.rds") 


##################################
#### COEFFICIENT OF VARIATION ####
expanel_cv <- expanel %>% 
  dplyr::select(variable, type, scenario_name, value, analised_sample, wave, id) %>% 
  dplyr::filter(variable!="prob", 
                type =="flow",
                wave=="wave2",
                analised_sample==1) %>% 
  dplyr::mutate(scenario_name = str_replace(scenario_name, "Scenario ", "")) %>% 
  group_by(id, variable) %>% 
  dplyr::summarise(value = mean(value, na.rm = T)) %>% 
  
  # log 
  
  dplyr::mutate(value_log = log(value))

# Flows

flows_forecast %>% 
  mutate(val_log = log(val)) %>% 
  group_by(flow) %>% 
  summarise(
    CV=
      cv_versatile(val_log, 
                   na.rm = T,
                   correction = T,
                   method = "norm")) %>% 
  as.data.frame()

# expert estimates

expanel_cv %>% 
  group_by(variable) %>% 
  summarise(
    CV=
      cv_versatile(value_log, 
                   na.rm = T,
                   correction = T,
                   method = "norm")) %>% 
  as.data.frame()

