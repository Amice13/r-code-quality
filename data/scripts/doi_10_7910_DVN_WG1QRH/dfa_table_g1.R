#### purpose: producing table G1 #### 

#### installing key packages #### 

list.of.packages = 
  c('readstata13', 'haven', 'tidyverse', 'dplyr', 'estimatr', 'texreg',
    'gridExtra', 'ggthemes', 'wCorr', 'questionr', 'xtable', 'sf', 
    'TAM', 'purrr', 'kable', 'kableExtra', 'wCorr', 'psych',
    'psychTools')
new.packages =  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#### libraries #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)    
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(estimatr)
    library(texreg)
    library(gridExtra)
    library(ggthemes)
    library(wCorr)
    library(questionr)
    library(xtable)
    library(sf)
    library(TAM)
    library(purrr)
    library(kable)
    library(kableExtra)
    library(wCorr)
    library(psych)
    library(psychTools)
    library(knitr)
    
  }
  
)

pew19 = read_spss("pew2020/W58_Dec19/ATP W58.sav")

pew19$eng_interview = ifelse(pew19$LANG_W58 == 1, 1, 0)

pew19$eng_dominance = 
  ifelse(pew19$LAN1_W58 == 99, NA, pew19$LAN1_W58) - 1 + 
  ifelse(pew19$LAN2_W58 == 99, NA, pew19$LAN2_W58) - 1 + 
  abs(ifelse(pew19$LAN3_W58 == 99, NA, pew19$LAN3_W58) - 4) + 
  abs(ifelse(pew19$LAN4_W58 == 99, NA, pew19$LAN4_W58) - 4)

pew19$eng_dominance = pew19$eng_dominance / max(pew19$eng_dominance, na.rm = TRUE)

engdom = lm_robust(eng_dominance ~ eng_interview, data = pew19, weight = WEIGHT_W58)

texreg(l = engdom,
       include.ci = FALSE,
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.coef.map = list("eng_interview" = "English Interview"),
       custom.model.names = "English Dominance",
       label = "table:engdomval",
       caption.above = TRUE,
       caption = "English Interview Indicator Proxies for English Dominance",
       float.pos = "!htbp")
