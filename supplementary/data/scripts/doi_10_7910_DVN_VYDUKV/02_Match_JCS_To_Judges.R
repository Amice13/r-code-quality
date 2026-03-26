#Set working directory to replication folder
#-----------------------------
#Packages necessary to run analyses
library(tidyverse)
library(readxl)


#Standardize FJC name
fjc <- read_excel('Data/Raw/FJC_Judge_Data.xlsx')
fjc$`Middle Name` = ifelse(is.na(fjc$`Middle Name`),' ',fjc$`Middle Name`)
fjc$Suffix = ifelse(is.na(fjc$Suffix),'',paste(' ',trimws(fjc$Suffix),sep=''))
fjc$name = paste(fjc$`Last Name`,', ',fjc$`First Name`,' ',fjc$`Middle Name`,',', fjc$Suffix,sep='')


JCS_COA <- read_csv('Data/Raw/jcs_nid.csv')
JCS_COA$coa=1
#Replace the nid for Kyle Duncan who is incorrectly assigned to the nid for Allyson Duncan
JCS_COA$nid[151]=4517676
JCS_COA <- JCS_COA %>%
  select(name, jcs_2022, circuit, nid,coa)



JCS_D <- read_csv('Data/Raw/Boyd_District_Court_JCS.csv')
cir_medians <- read_csv('Data/Raw/jcs_medians.csv')
cir_medians <- cir_medians %>%
  select('year','1','2','3','4','5','6','7','8','9','10','11','DC') %>%
  pivot_longer(-year,values_to='cir_median',names_to='Circuit') %>%
  mutate(year=as.character(year))
  
#Match FJC to ideology
fjc <- fjc %>%
  left_join(JCS_COA, by = 'nid') %>%
  left_join(JCS_D, by = 'nid') %>%
  mutate(coa = ifelse(is.na(coa),0,1),
    ideology = as.numeric(ifelse(coa==1,jcs_2022,ideology_score))) %>%
  select(name.x,nid,jid,ideology) %>%
  drop_na() %>%
  unique()

colnames(fjc)[1]='name'


#Match ideology to judges and calculate panel median
data <- read_xlsx('Data/Raw/Taboni_COFI_Cases.xlsx')

case_matches <- data %>%
  left_join(fjc, by = c('Judge 1'='name'), relationship = 'many-to-one')%>%
  left_join(fjc, by = c('Judge 2'='name'), relationship = 'many-to-one') %>%
  left_join(fjc, by = c('Judge 3'='name'), relationship = 'many-to-one')   %>%
  left_join(cir_medians, by = c('Circuit','Publication Year'='year')) %>%
  rowwise()%>%
  mutate(`Median Ideology` =ifelse(`En Banc`==0,
                 as.numeric(median(c(ideology,ideology.x,ideology.y),na.rm=F)),
                          cir_median))

#Load hand codings for panels that did not properly match
hand_coded_medians <- read_csv('Data/Raw/Hand_Coded_Medians.csv')
hand_coded_medians$fix=1
case_matches <- case_matches %>%
  left_join(hand_coded_medians, by = c('Issue','Citation')) %>%
  mutate(`Median Ideology`=ifelse(!is.na(fix), `Median Ideology.y`,`Median Ideology.x`)) %>%
  select(Issue, Citation, Circuit, Position, `Judge 1`, `Judge 2`, `Judge 3`,
         `Publication Year`, `En Banc`, Liberal, Occurrence, `Median Ideology`)


#Write analysis data set
writexl::write_xlsx(case_matches,'Data/Taboni_COFI_Analysis.xlsx')



