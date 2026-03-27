## THIS SCRIPT OUTPUTS THE SUMMARY COUNTS OF UNITS ACROSS FIXED EFFECT LEVELS.
## OUTPUTS TABLE S1 and S2.

## SET WD

## LOAD PACKAGES
require(tidyverse)
require(xtable)
require(gtools)


## LOAD DATA
load("02-data/befm-main-analysis.Rdata")
df = as.data.table(df)
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31),]

##############################
####### MAKE VARIABLES #######
####### FOR ANALYSIS   #######
##############################


df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR

#### FIXED EFFECT VARIABLES

# county
df$county_state = paste0(df$statefip, "_", df$county)  

# enumdist
df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)
# 10 page
df$page10 = round(df$pageno, digits = -1)
df$reel_page10 = paste0(df$reel, "_", df$page10)

# 5 page
mround <- function(x,base){ 
  base*round(x/base) }
df$page5 = mround(df$pageno, 5)
df$reel_page5 = paste0(df$reel, "_", df$page5)

fe = df %>% 
  as_tibble %>%
  filter(white==1)%>%
  dplyr::select(opposite1_dist_1, statefip, county_state, reel,  cnty_enumdist, reel_page10, reel_page5)%>%
  gather(key = fe, value = fe_code, -opposite1_dist_1)%>%
  group_by(fe, fe_code)%>%
  filter(!is.na(fe_code))%>%
  dplyr::summarize(n = dplyr::n(),
            blackneighbor = sum(opposite1_dist_1 == 1, na.rm=T))%>%
  group_by(fe) %>%
  dplyr::summarize(N = dplyr::n(),
            N2 = sum(blackneighbor > 0 & n > blackneighbor, na.rm = T),
            effective = sum(n[blackneighbor > 0 & n > blackneighbor], na.rm = T),
            effective_blackneighbor = sum(blackneighbor[blackneighbor > 0 & n > blackneighbor], na.rm = T))%>%
  mutate(fe = factor(fe, levels = c('statefip', 
                                    'county_state',
                                    'cnty_enumdist',
                                    'reel',
                                    'reel_page10',
                                    'reel_page5')))

tab = fe %>% 
  arrange(fe) %>%
  xtable


  writeLines(capture.output(tab), "03-output/02-tables/TabS1.tex")


  rm(list=ls())
  
  
  
  
load("02-data/seg_analysis_2017.Rdata")
  df = as.data.table(df)

  ##############################
  ####### MAKE VARIABLES #######
  ####### FOR ANALYSIS   #######
  ##############################

  df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR
  
  #### FIXED EFFECT VARIABLES
  
  # county
  df$county_state = paste0(df$statefip, "_", df$county)  
  

  # enumdist
  df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
  df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)
  # 10 page
  df$page10 = round(df$pageno, digits = -1)
  df$reel_page10 = paste0(df$reel, "_", df$page10)
  
  # 5 page
  mround <- function(x,base){ 
    base*round(x/base) }
  df$page5 = mround(df$pageno, 5)
  df$reel_page5 = paste0(df$reel, "_", df$page5)
  
  fe = df %>% 
    as_tibble %>%
    filter(white==1)%>%
    dplyr::select(opposite1_dist_1, statefip, county_state, reel,  cnty_enumdist, reel_page10, reel_page5)%>%
    gather(key = fe, value = fe_code, -opposite1_dist_1)%>%
    group_by(fe, fe_code)%>%
    filter(!is.na(fe_code))%>%
    dplyr::summarize(n = dplyr::n(),
              blackneighbor = sum(opposite1_dist_1 == 1, na.rm=T))%>%
    group_by(fe) %>%
    dplyr::summarize(N = dplyr::n(),
              N2 = sum(blackneighbor > 0 & n > blackneighbor, na.rm = T),
              effective = sum(n[blackneighbor > 0 & n > blackneighbor], na.rm = T),
              effective_blackneighbor = sum(blackneighbor[blackneighbor > 0 & n > blackneighbor], na.rm = T))%>%
    mutate(fe = factor(fe, levels = c('statefip', 
                                      'county_state',
                                      'cnty_enumdist',
                                      'reel',
                                      'reel_page10',
                                      'reel_page5'
)))
  
  tab = fe %>% 
    arrange(fe) %>%
    xtable
  
  writeLines(capture.output(tab), "03-output/02-tables/TabS2.tex")
  
  