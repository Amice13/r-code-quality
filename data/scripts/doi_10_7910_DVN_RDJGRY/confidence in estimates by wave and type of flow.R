source('scripts/cleaning_and_management/libraries.R') 
# Code for split violin plots and adjusted labels

source('scripts/cleaning_and_management/split_violin_and_labels.R') 

# read data

expanel <- readRDS("data/Delphi survey/expanel.rds") 


#################################
#### CONFIDENCE IN ESTIMATES ####

# by wave 

expanel %>% 
  filter(analised_sample==1, type=="conf",panel=="1") %>% 
  group_by(wave) %>% 
  summarise(mean=mean(value,na.rm = T),
            sd=sd(value,na.rm = T),
            median=median(value,na.rm = T),
            iqr = IQR(value,na.rm = T))

# by wave and scenario

expanel %>% 
  filter(analised_sample==1, type=="conf",panel=="1") %>% 
  group_by(variable, wave) %>% 
  summarise(mean=mean(value,na.rm = T),
            sd=sd(value,na.rm = T),
            median=median(value,na.rm = T),
            iqr = IQR(value,na.rm = T))
