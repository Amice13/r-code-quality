rm(list=ls())
# Function to load packages
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    {install.packages(lib, repos='http://cran.rstudio.com/')}
    suppressMessages( library(lib, character.only=TRUE))}}

# Load libraries
packs=c("cshapes","readstata13", "DataCombine", "countrycode","arm", "rgeos", "raster", "tidyr", "pwt9",
        "texreg", "reshape2","ggplot2", 'foreign', 'car', 'lme4', 'dplyr', "gridExtra","readr","readr",
        "pscl", "stargazer","stringi","multiwayvcov", "lmtest","scales","stringi","haven","readxl",
        "coefplot", "ggthemes", "sf", "tidyverse","dplyr","extrafont","showtext","ggmap","xtable",
        "spduration")
loadPkg(packs)

#show chinese characters
showtext_auto(enable = TRUE)
#font_add('SimSun', 'SimSun.ttf')
font_add('KaiTi', 'KaiTi.ttf')

## load data
load("./Clean_data/data_full.RData")
load("./Clean_data/data_train.RData") #for model evaluation
load("./Clean_data/data_test.RData") # for out-of-sample prediction

data_full <- data_full %>%
            dplyr::group_by(gwno, year)%>%
            dplyr::mutate(cty_terr_total = sum(nterr),
                          cty_terr_total_t1 = sum(nterr_t1))%>%
            dplyr::ungroup()%>%
            dplyr::group_by(year)%>%
            dplyr::mutate(year_terr_total = sum(nterr),
                          year_terr_total_t1 = sum(nterr_t1))%>%
            dplyr::mutate(af_totalexcl = year_terr_total - cty_terr_total,
                          af_totalexcl_t1 = year_terr_total_t1 - cty_terr_total_t1) %>%
  dplyr::ungroup()
  


#### lag all IVs
data_full <- data_full %>%
            dplyr::mutate(neighboringInterStateWar_t1 = lag(neighboringInterStateWar, 1),
                          neighboringcivilwar_t1 = lag(neighboringcivilwar, 1),
                          polity2_t1 = lag(polity2, 1),
                          lngdppc_t1 = lag(lngdppc, 1),
                          lntpop_t1 = lag(lntpop, 1),
                          milexpr_t1 = lag(milexpr, 1),
                          irregular_dummy_t1 = lag(irregular_dummy, 1),
                          number.total.conflict_t1 = lag(number.total.conflict, 1),
                          nlights_max_t1 = lag(nlights_max, 1),
                          excluded_t1 = lag(excluded, 1),
                          state_vi_t1 = lag(state_vi, 1))%>%
                 filter(year > 1993)
#notice that in the GTD all records of terrorist attacks during 1993 were lost


## DV: duration & atrisk
## IVs: 
# subnational:  nlights_max, mountains_mean, capdist,bdist2, excluded, state_vi,petroleum_s
# country-level: polity2, lngdppc, lntpop, milexpr, irregular_dummy, number.total.conflict, 
# international; neighboringInterStateWar, neighboringcivilwar, year_terr_total cty_terr_total
## spatial lags: nterr_s1, nterr_s2
data_full <- as.data.frame(data_full)

#######################################################################################################
##  Table 1:
#######################################################################################################
#summary
library(stargazer)
sum_df <-data_full %>% 
                  select(neighboringInterStateWar_t1, neighboringcivilwar_t1,  
                           year_terr_total_t1 , cty_terr_total_t1 , polity2_t1 , 
                           lngdppc_t1 , lntpop_t1 , milexpr_t1 , irregular_dummy_t1 , 
                           number.total.conflict_t1 , nlights_max_t1 , mountains_mean ,
                           capdist , bdist2 , excluded_t1 , state_vi_t1 , petroleum_s ,
                           nterr_t1 , nterr_s1 ,  nterr_s2)
stargazer(sum_df, type = "latex", title = "", summary.stat = c("n","mean","sd","min","max"),
          out = "TEX/tables/summary.tex")
#######################################################################################################





#main model
lag_spdur_wb <- spdur(
               duration ~ neighboringInterStateWar_t1 + neighboringcivilwar_t1 + 
                          year_terr_total_t1 + cty_terr_total_t1 + polity2_t1 + 
                          lngdppc_t1 + lntpop_t1 + milexpr_t1 + irregular_dummy_t1 + 
                          number.total.conflict_t1 + nlights_max_t1 + mountains_mean +
                          capdist + bdist2 + excluded_t1 + state_vi_t1 + petroleum_s +
                          nterr_t1 + nterr_s1 +  nterr_s2,
                atrisk ~ neighboringInterStateWar_t1 + neighboringcivilwar_t1 + 
                         year_terr_total_t1 + cty_terr_total_t1 + polity2_t1 + 
                         lngdppc_t1 + lntpop_t1 + milexpr_t1 + irregular_dummy_t1 + 
                         number.total.conflict_t1 + nlights_max_t1 + mountains_mean +
                         capdist + bdist2 + excluded_t1 + state_vi_t1 + petroleum_s +
                         nterr_t1 + nterr_s1 +  nterr_s2,
              data = data_full, distr = "weibull", silent = TRUE)


spdur_wb <- spdur(
                duration ~ neighboringInterStateWar + neighboringcivilwar + 
                          year_terr_total + cty_terr_total + polity2 + 
                          lngdppc + lntpop + milexpr + irregular_dummy + 
                          number.total.conflict + nlights_max + mountains_mean +
                          capdist + bdist2 + excluded + state_vi + petroleum_s +
                          nterr_t1 + nterr_s1 +  nterr_s2,
                atrisk ~ neighboringInterStateWar + neighboringcivilwar + 
                          year_terr_total + cty_terr_total + polity2 + 
                          lngdppc + lntpop + milexpr + irregular_dummy + 
                          number.total.conflict + nlights_max + mountains_mean +
                          capdist + bdist2 + excluded + state_vi + petroleum_s +
                          nterr_t1 + nterr_s1 +  nterr_s2,
                data = data_full, distr = "weibull", silent = TRUE)

# log-log
lag_spdur_loglog <- spdur(
                  duration ~ neighboringInterStateWar_t1 + neighboringcivilwar_t1 + 
                              year_terr_total_t1 + cty_terr_total_t1 + polity2_t1 + 
                              lngdppc_t1 + lntpop_t1 + milexpr_t1 + irregular_dummy_t1 + 
                              number.total.conflict_t1 + nlights_max_t1 + mountains_mean +
                              capdist + bdist2 + excluded_t1 + state_vi_t1 + petroleum_s +
                              nterr_t1 + nterr_s1 +  nterr_s2,
                  atrisk ~ neighboringInterStateWar_t1 + neighboringcivilwar_t1 + 
                          year_terr_total_t1 + cty_terr_total_t1 + polity2_t1 + 
                          lngdppc_t1 + lntpop_t1 + milexpr_t1 + irregular_dummy_t1 + 
                          number.total.conflict_t1 + nlights_max_t1 + mountains_mean +
                          capdist + bdist2 + excluded_t1 + state_vi_t1 + petroleum_s +
                          nterr_t1 + nterr_s1 +  nterr_s2,
                data = data_full, distr = "loglog", silent = TRUE)

## no lag for loglog

spdur_loglog <- spdur(
                  duration ~ neighboringInterStateWar + neighboringcivilwar + 
                              year_terr_total + cty_terr_total + polity2 + 
                              lngdppc + lntpop + milexpr + irregular_dummy + 
                              number.total.conflict + nlights_max + mountains_mean +
                              capdist + bdist2 + excluded + state_vi + petroleum_s +
                              nterr_t1 + nterr_s1 +  nterr_s2,
                  atrisk ~ neighboringInterStateWar + neighboringcivilwar + 
                            year_terr_total + cty_terr_total + polity2 + 
                            lngdppc + lntpop + milexpr + irregular_dummy + 
                            number.total.conflict + nlights_max + mountains_mean +
                            capdist + bdist2 + excluded + state_vi + petroleum_s +
                            nterr_t1 + nterr_s1 +  nterr_s2,
                  data = data_full, distr = "loglog", silent = TRUE)

#######################################################################################################
#######################################################################################################
# save all results
save(lag_spdur_wb, spdur_wb, lag_spdur_loglog, spdur_loglog, file = "./Clean_data/models.RData")
#######################################################################################################
#######################################################################################################





## make latex tables
source("Code_paper/print_SPDMtable.R")

spdm_table(object = lag_spdur_wb, noModels = 5, digs= 4, caption = "Weibu with lag",
           label = "tab:coefwblag", file = "TEX/tables/lag_spdur_wb.tex")

spdm_table(object = spdur_wb, noModels = 5, digs= 4, caption = "Weibu without lag",
           label = "tab:coefwb", file = "TEX/tables/spdur_wb.tex")

spdm_table(object = lag_spdur_loglog, noModels = 5, digs= 4, caption = "Loglog with lag",
           label = "tab:coefloglag", file = "TEX/tables/lag_spdur_loglog.tex")
spdm_table(object = spdur_loglog, noModels = 5, digs= 4, caption = "Loglog without lag",
           label = "tab:coeflog", file = "TEX/tables/spdur_loglog.tex")


##############################################################################################################################
### in-sample-prediction using loglog
data_full$crisk <- predict(spdur_loglog, type = "conditional risk", newdata = data_full, na.action = "na.exclude")
data_full$ccure <- predict(spdur_loglog, type = "conditional cure", newdata = data_full, na.action = "na.exclude")
data_full$chazard <- predict(spdur_loglog, type = "conditional hazard", newdata = data_full, na.action = "na.exclude")
insample_df <- data_full %>% 
              dplyr::select(gid, year,gwno, row, col, xcoord, ycoord, terr_dum, 
                duration, failure, ongoing, end.spell, cured,
                atrisk,censor,t.0, crisk, chazard, ccure)
save(insample_df, file = "./Clean_data/insample_df.RData")


##############################################################################################################################
#
### out-of-sample prediction
#######################################################################################################

data_train <- data_train  %>%
  dplyr::group_by(gwno, year)%>%
  dplyr::mutate(cty_terr_total = sum(nterr),
                cty_terr_total_t1 = sum(nterr_t1))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(year)%>%
  dplyr::mutate(year_terr_total = sum(nterr),
                year_terr_total_t1 = sum(nterr_t1))%>%
  dplyr::mutate(af_totalexcl = year_terr_total - cty_terr_total,
                af_totalexcl_t1 = year_terr_total_t1 - cty_terr_total_t1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(neighboringInterStateWar_t1 = lag(neighboringInterStateWar, 1),
                neighboringcivilwar_t1 = lag(neighboringcivilwar, 1),
                polity2_t1 = lag(polity2, 1),
                lngdppc_t1 = lag(lngdppc, 1),
                lntpop_t1 = lag(lntpop, 1),
                milexpr_t1 = lag(milexpr, 1),
                irregular_dummy_t1 = lag(irregular_dummy, 1),
                number.total.conflict_t1 = lag(number.total.conflict, 1),
                nlights_max_t1 = lag(nlights_max, 1),
                excluded_t1 = lag(excluded, 1),
                state_vi_t1 = lag(state_vi, 1))%>%
  filter(year > 1993)


data_test <- data_test %>%
  dplyr::group_by(gwno, year)%>%
  dplyr::mutate(cty_terr_total = sum(nterr),
                cty_terr_total_t1 = sum(nterr_t1))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(year)%>%
  dplyr::mutate(year_terr_total = sum(nterr),
                year_terr_total_t1 = sum(nterr_t1))%>%
  dplyr::mutate(af_totalexcl = year_terr_total - cty_terr_total,
                af_totalexcl_t1 = year_terr_total_t1 - cty_terr_total_t1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(neighboringInterStateWar_t1 = lag(neighboringInterStateWar, 1),
                neighboringcivilwar_t1 = lag(neighboringcivilwar, 1),
                polity2_t1 = lag(polity2, 1),
                lngdppc_t1 = lag(lngdppc, 1),
                lntpop_t1 = lag(lntpop, 1),
                milexpr_t1 = lag(milexpr, 1),
                irregular_dummy_t1 = lag(irregular_dummy, 1),
                number.total.conflict_t1 = lag(number.total.conflict, 1),
                nlights_max_t1 = lag(nlights_max, 1),
                excluded_t1 = lag(excluded, 1),
                state_vi_t1 = lag(state_vi, 1))%>%
  filter(year > 1993)


data_test <- as.data.frame(data_test)
data_train <- as.data.frame(data_train)
## fit model with training data
train_spdur_loglog <- spdur(
                  duration ~ neighboringInterStateWar + neighboringcivilwar + 
                    year_terr_total + cty_terr_total + polity2 + 
                    lngdppc + lntpop + milexpr + irregular_dummy + 
                    number.total.conflict + nlights_max + mountains_mean +
                    capdist + bdist2 + excluded + state_vi + petroleum_s +
                    nterr_t1 + nterr_s1 +  nterr_s2,
                  atrisk ~ neighboringInterStateWar + neighboringcivilwar + 
                    year_terr_total + cty_terr_total + polity2 + 
                    lngdppc + lntpop + milexpr + irregular_dummy + 
                    number.total.conflict + nlights_max + mountains_mean +
                    capdist + bdist2 + excluded + state_vi + petroleum_s +
                    nterr_t1 + nterr_s1 +  nterr_s2,
                  data = data_train, distr = "loglog", silent = TRUE)

save(train_spdur_loglog, file = "./Clean_data/train_spdur_loglog.RData")
spdm_table(object = train_spdur_loglog, noModels = 5, digs= 4, caption = "training set Loglog with no lag",
           label = "tab:coeflogtrain", file = "TEX/tables/spdur_loglog_train.tex")

#out-of-sample prediction
data_test$crisk <- predict(train_spdur_loglog, type = "conditional risk", newdata = data_test, na.action = "na.exclude")
data_test$ccure <- predict(train_spdur_loglog, type = "conditional cure", newdata = data_test, na.action = "na.exclude")
data_test$chazard <- predict(train_spdur_loglog, type = "conditional hazard", newdata = data_test, na.action = "na.exclude")

## get 
outsample_df <- data_test %>% 
              dplyr::select(gid, year,gwno, row, col, xcoord, ycoord, terr_dum, 
                            duration, failure, ongoing, end.spell, cured,
                            atrisk,censor,t.0, crisk, chazard, ccure)
save(outsample_df, file = "./Clean_data/outsample_df.RData")

## create a complete case data fram based on model 
test_complete <- data_test %>%
                 dplyr::select(gid, year,gwno, row, col, xcoord, ycoord, terr_dum, 
                         duration, nlights_max, mountains_mean, capdist, bdist2, excluded, state_vi,petroleum_s,
                         nterr_s1, nterr_s2, atrisk, neighboringInterStateWar, neighboringcivilwar,
                         year_terr_total, cty_terr_total, polity2, 
                         lngdppc, lntpop, milexpr, irregular_dummy, number.total.conflict)
## get a complete case data
test_complete <- test_complete[complete.cases(test_complete),]    

#######################################################################################################
## add predicted probs into the data
outofsample_pred <- cbind(test_complete, data_test$chazard)
save(outofsample_pred, file = "./Clean_data/outofsample_pred.RData")
#######################################################################################################


