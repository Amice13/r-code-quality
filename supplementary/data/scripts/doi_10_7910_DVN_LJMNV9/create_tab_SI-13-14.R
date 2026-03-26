# This R-file is generating the table SI-13-14

## ----setup, include=FALSE------------------------------------------------
#knitr::opts_chunk$set(echo = FALSE)

rm(list=ls())
## ----setup content, include=FALSE, echo=FALSE, cache=FALSE---------------
library(textreg);library(pder);library(tidyverse);library(ggrepel);library(broom);library(margins);library(stargazer);library(modelr)
library(ggthemes);library(plm);library(readstata13);library(gridExtra);library(visreg);library(memisc);library(splines)
library(ggeffects);library(modelr);library(pglm);library(lmtest);library(lfe);library(clubSandwich);library(jtools)
library(ggstance);library(interflex);library(ggthemes);library(clusterSEs);library(AER)

#my added libraries -- start
library(sp)
library(maptools)
library(rgdal)
library(spdep)
library(classInt)
library(dplyr)
library(tidyr)
library(sf)
library(caret)
library(knitr)
library(kableExtra)
library(xtable)
#my added libraries -- end

#setwd("~/Dropbox (Mitts)/Projects/Israeli Newspapers Study/")
setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

####### mydata <- read.dta13("Data/clean/ready_arealong.dta")
mydata <- read.dta13('Data/arealong_final_SI13-14.dta')
names(mydata)

table(mydata$area_num)

################################################
#### Original Data Prep:
################################################

# subset data; assign israelhayom_percent =0 pre-launch; and drop Eilat and "Other"
IH <-
  mydata %>% dplyr::select(
    area_num,
    year,
    voters,
    israelhayom_percent,
    Plikud,
    Plabor,
    Pisrael_beitenu,
    Pkadima,
    Pbaityehudi,
    Pshas,
    Pmerez,
    ALL_rightbloc,
    RIGHTBLOC,
    BIG_rightbloc,
    laborforcey_pcnt_08,
    religion_pcnt_08,
    age_median_08,
    africa_pcnt_08,
    europe_pcnt_08,
    matriccert_pcnt_08,
    national_pop,
    secular_mean,
    secular_max,
    travel_dist
  ) %>%
  replace_na(list(israelhayom_percent = 0)) %>%
  replace_na(list(BIG_rightbloc = 0)) %>%
  replace_na(list(religion_pcnt_08 = 0)) %>%
  replace_na(list(africa_pcnt_08 = 0)) %>%
  replace_na(list(matriccert_pcnt_08 = 0)) %>%
  replace_na(list(Plikud = 0)) %>%
  group_by(area_num) %>%
  mutate(yr = row_number()) %>% dplyr::filter(area_num < 27)

IH$IH08 <- NA
for (i in 1:length(unique(IH$area_num))){
  IH$IH08[IH$area_num==i] <- IH$israelhayom_percent[IH$year==2009 & IH$area_num==i]
}

IH$pop <- NA
for (i in 1:length(unique(IH$area_num))){
  IH$pop[IH$area_num==i] <- IH$national_pop[IH$year==2009 & IH$area_num==i]
}

################################################
#### My Data Prep, create cross-sections as well as full dataset:
################################################

#read in shapefile of regions
media_markets_yshuv_import <- readOGR(dsn = "Data/media_markets_yshuv/", layer = "media_markets_yshuv")
#create weight matrices of contiguous queen neighbors (both of the below matrices are acceptable)
queens_nbrs <- poly2nb(media_markets_yshuv_import, row.names = media_markets_yshuv_import$num_area, queen = FALSE)
queens_nbrs_std <- nb2listw(queens_nbrs)
#convert shapefile to sf object for general usabliity with sf tools
media_markets_yshuv <- st_as_sf(media_markets_yshuv_import)
#some data cleaning, creating a factor version of area number for ease of use
IH_mod <-
  IH %>%
  mutate(area_num_fac = as.factor(area_num))
#Adding spatial data to the dataset
IH_mod <- left_join(IH_mod,media_markets_yshuv, by = c("area_num_fac"="Id"))
st_as_sf(IH_mod)
#Calculating and joining to dataset each market's neighbors' values for IsraelHayom_Percent and All_rightbloc and BIG_rightbloc
IH_mod <-
  IH_mod %>%
  left_join(.,as.data.frame(rbind(cbind(qn_Israel = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2009))$israelhayom_percent), area_num = c(1:25), year = 2009),
                                  cbind(qn_Israel = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2013))$israelhayom_percent), area_num = c(1:25), year = 2013),
                                  cbind(qn_Israel = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2015))$israelhayom_percent), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year")) %>%
  left_join(.,as.data.frame(rbind(cbind(qn_Plikud = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 1996))$Plikud), area_num = c(1:25), year = 1996),
                                  cbind(qn_Plikud = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 1999))$Plikud), area_num = c(1:25), year = 1999),
                                  cbind(qn_Plikud = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2003))$Plikud), area_num = c(1:25), year = 2003),
                                  cbind(qn_Plikud = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2006))$Plikud), area_num = c(1:25), year = 2006),
                                  cbind(qn_Plikud = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2009))$Plikud), area_num = c(1:25), year = 2009),
                                  cbind(qn_Plikud = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2013))$Plikud), area_num = c(1:25), year = 2013),
                                  cbind(qn_Plikud = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2015))$Plikud), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year")) %>%
  left_join(.,as.data.frame(rbind(cbind(qn_BIG_rightbloc = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 1996))$BIG_rightbloc), area_num = c(1:25), year = 1996),
                                  cbind(qn_BIG_rightbloc = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 1999))$BIG_rightbloc), area_num = c(1:25), year = 1999),
                                  cbind(qn_BIG_rightbloc = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2003))$BIG_rightbloc), area_num = c(1:25), year = 2003),
                                  cbind(qn_BIG_rightbloc = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2006))$BIG_rightbloc), area_num = c(1:25), year = 2006),
                                  cbind(qn_BIG_rightbloc = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2009))$BIG_rightbloc), area_num = c(1:25), year = 2009),
                                  cbind(qn_BIG_rightbloc = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2013))$BIG_rightbloc), area_num = c(1:25), year = 2013),
                                  cbind(qn_BIG_rightbloc = lag.listw(queens_nbrs_std, (IH_mod %>% filter(year == 2015))$BIG_rightbloc), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year")) %>%
  #mutate(israelhayom_percent = na_if(israelhayom_percent,0)) %>%
  ungroup() %>%
  mutate(indexID = row_number())
#mutate(qn_Israel = ifelse(is.na(qn_Israel)==TRUE,NULL,qn_Israel))


test.elections <- c("IH2009", "IH2012", "IH2015")
yr.elections = c(7,11,13)
for (i in 1:length(test.elections)) {
  y <- yr.elections[i]
  temp <- IH_mod %>% group_by(area_num) %>%
    summarize(
      ALL_rightbloc0 = mean(ALL_rightbloc[yr < 7], na.rm=T),
      ALL_rightbloc1 = mean(ALL_rightbloc[yr == y], na.rm=T),
      RIGHTBLOC0 = mean(RIGHTBLOC[yr < 7], na.rm=T),
      RIGHTBLOC1 = mean(RIGHTBLOC[yr == y], na.rm=T),
      BIG_rightbloc0 = mean(BIG_rightbloc[yr < 7], na.rm=T),
      BIG_rightbloc1 = mean(BIG_rightbloc[yr == y], na.rm=T),
      likud0 = mean(Plikud[yr < 7], na.rm=T),
      likud1 = mean(Plikud[yr == y], na.rm=T),
      labor0 = mean(Plabor[yr < 7], na.rm=T),
      labor1 = mean(Plabor[yr == y], na.rm=T),
      shas0 = mean(Pshas[yr < 7], na.rm=T),
      shas1 = mean(Pshas[yr == y], na.rm=T),
      kadima0 = mean(Pkadima[yr < 7], na.rm = T),
      kadima1 = mean(Pkadima[yr == y], na.rm=T),
      IB0 = mean(Pisrael_beitenu[yr < 7], na.rm = T),
      IB1 = mean(Pisrael_beitenu[yr == 7], na.rm=T),
      merez0 = mean(Pmerez[yr < 7], na.rm=T),
      merez1 = mean(Pmerez[yr == y], na.rm=T),
      treat = israelhayom_percent[yr == y],
      pop = voters[yr == 7],
      religion = religion_pcnt_08[yr == 7],
      age = age_median_08[yr == 7],
      africa = africa_pcnt_08[yr == 7],
      europe = europe_pcnt_08[yr == 7],
      matric = matriccert_pcnt_08[yr == 7],
      secular_mean = secular_mean[yr == 7],
      secular_max = secular_max[yr == 7],
      travel_dist =  travel_dist[yr == 7],
      qn_Israel_ = qn_Israel[yr == y],
      qn_Israel_09 = qn_Israel[yr == 7],
      qn_Israel_13 = qn_Israel[yr == 11],
      qn_Israel_15 = qn_Israel[yr == 13],
      qn_likud0 = mean(Plikud[yr < 7]),
      qn_likud_ = Plikud[yr == y],
      qn_likud09 = Plikud[yr == 7],
      qn_likud13 = Plikud[yr == 11],
      qn_likud15 = Plikud[yr == 13],
      qn_BIG_rightbloc0 = mean(BIG_rightbloc[yr < 5], na.rm=T),
      qn_BIG_rightbloc_ = BIG_rightbloc[yr == y],
      qn_BIG_rightbloc09 = BIG_rightbloc[yr == 7],
      qn_BIG_rightbloc13 = BIG_rightbloc[yr == 11],
      qn_BIG_rightbloc15 = BIG_rightbloc[yr == 13]
    ) %>% mutate(
      dALL_rightbloc = ALL_rightbloc1 - ALL_rightbloc0,
      dRIGHTBLOC = RIGHTBLOC1 - RIGHTBLOC0,
      dBIG_rightbloc = BIG_rightbloc1 - BIG_rightbloc0,
      dlikud = likud1 - likud0,
      dlabor = labor1 - labor0,
      dshas = shas1 - shas0,
      dkadima = kadima0,
      dIB = IB1 - IB0
      #,d_qn_Israel = ifelse(y == 5,NULL,ifelse(y==6,qn_Israel_13-qn_Israel_09,qn_Israel_15-qn_Israel_09))
    )
  temp$right <- ifelse(temp$BIG_rightbloc0>median(temp$BIG_rightbloc0), 1, 0)
  assign(test.elections[i], temp)
  rm(temp, y)
}

################################################
#### Cross-Sectional Analysis:
################################################

### Define dependent variables and covariates for cross-sectional regressions ---------------

dv <- c("dRIGHTBLOC", "dBIG_rightbloc", "dlikud", "dlabor", "dshas", "dkadima")
dv2 <- c("Right Bloc", "Big Right Bloc", "Likud", "Labor", "Shas", "Kadima")
dv3 <- c("dBIG_rightbloc", "dlikud")

covariates <-"log(pop) + religion + europe + matric"
spatial_lag_model_var <- "qn_Israel_"
spatial_error_model_var <- "qn_resids"

#### Run cross-sectional regressions -----------------------------------------

##dBig_Rightbloc

#2009
lm_09_OLS_Brb_bare <- lm(as.formula(paste("dBIG_rightbloc ~ treat")), IH2009, weights = pop)
summary(lm_09_OLS_Brb_bare)
lm_09_OLS_Brb_bare_Moran <- moran.test((cbind(resids = lm_09_OLS_Brb_bare$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_09_OLS_Brb_wCov <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",covariates,sep = "")), IH2009, weights = pop)
summary(lm_09_OLS_Brb_wCov)
lm_09_OLS_Brb_wCov_Moran <- moran.test((cbind(resids = lm_09_OLS_Brb_wCov$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_09_OLS_Brb_spLag <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",spatial_lag_model_var,sep = "")), IH2009, weights = pop)
summary(lm_09_OLS_Brb_spLag)
lm_09_OLS_Brb_spLag_Moran <- moran.test((cbind(resids = lm_09_OLS_Brb_spLag$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_09_OLS_Brb_spLag_wCov <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",covariates," + ",spatial_lag_model_var,sep = "")), IH2009, weights = pop)
summary(lm_09_OLS_Brb_spLag_wCov)
lm_09_OLS_Brb_spLag_wCov_Moran <- moran.test((cbind(resids = lm_09_OLS_Brb_spLag_wCov$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value

IH2009_Brb_wResid <- cbind(resids_OLS = lm_09_OLS_Brb_bare$residuals,IH2009)
IH2009_Brb_wResid <- IH2009_Brb_wResid %>% mutate(qn_resids_Brb = lag.listw(queens_nbrs_std, resids_OLS))

lm_09_OLS_Brb_spErr <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ","qn_resids_Brb"," + ",covariates,sep = "")), IH2009_Brb_wResid, weights = pop)
summary(lm_09_OLS_Brb_spErr)
lm_09_OLS_Brb_spErr_Moran <- moran.test((cbind(resids_ = lm_09_OLS_Brb_spErr$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids_, queens_nbrs_std)$p.value

#2012
lm_12_OLS_Brb_bare <- lm(as.formula(paste("dBIG_rightbloc ~ treat")), IH2012, weights = pop)
summary(lm_12_OLS_Brb_bare)
lm_12_OLS_Brb_bare_Moran <- moran.test((cbind(resids = lm_12_OLS_Brb_bare$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_12_OLS_Brb_wCov <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",covariates,sep = "")), IH2012, weights = pop)
summary(lm_12_OLS_Brb_wCov)
lm_12_OLS_Brb_wCov_Moran <- moran.test((cbind(resids = lm_12_OLS_Brb_wCov$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_12_OLS_Brb_spLag <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",spatial_lag_model_var,sep = "")), IH2012, weights = pop)
summary(lm_12_OLS_Brb_spLag)
lm_12_OLS_Brb_spLag_Moran <- moran.test((cbind(resids = lm_12_OLS_Brb_spLag$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_12_OLS_Brb_spLag_wCov <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",covariates," + ",spatial_lag_model_var,sep = "")), IH2012, weights = pop)
summary(lm_12_OLS_Brb_spLag_wCov)
lm_12_OLS_Brb_spLag_wCov_Moran <- moran.test((cbind(resids = lm_12_OLS_Brb_spLag_wCov$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value

IH2012_Brb_wResid <- cbind(resids_OLS = lm_12_OLS_Brb_bare$residuals,IH2012)
IH2012_Brb_wResid <- IH2012_Brb_wResid %>% mutate(qn_resids_Brb = lag.listw(queens_nbrs_std, resids_OLS))

lm_12_OLS_Brb_spErr <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ","qn_resids_Brb"," + ",covariates,sep = "")), IH2012_Brb_wResid, weights = pop)
summary(lm_12_OLS_Brb_spErr)
lm_12_OLS_Brb_spErr_Moran <- moran.test((cbind(resids_ = lm_12_OLS_Brb_spErr$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids_, queens_nbrs_std)$p.value

#2015
lm_15_OLS_Brb_bare <- lm(as.formula(paste("dBIG_rightbloc ~ treat")), IH2015, weights = pop)
summary(lm_15_OLS_Brb_bare)
lm_15_OLS_Brb_bare_Moran <- moran.test((cbind(resids = lm_15_OLS_Brb_bare$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_15_OLS_Brb_wCov <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",covariates,sep = "")), IH2015, weights = pop)
summary(lm_15_OLS_Brb_wCov)
lm_15_OLS_Brb_wCov_Moran <- moran.test((cbind(resids = lm_15_OLS_Brb_wCov$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_15_OLS_Brb_spLag <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",spatial_lag_model_var,sep = "")), IH2015, weights = pop)
summary(lm_15_OLS_Brb_spLag)
lm_15_OLS_Brb_spLag_Moran <- moran.test((cbind(resids = lm_15_OLS_Brb_spLag$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_15_OLS_Brb_spLag_wCov <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ",covariates," + ",spatial_lag_model_var,sep = "")), IH2015, weights = pop)
summary(lm_15_OLS_Brb_spLag_wCov)
lm_15_OLS_Brb_spLag_wCov_Moran <- moran.test((cbind(resids = lm_15_OLS_Brb_spLag_wCov$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value

IH2015_Brb_wResid <- cbind(resids_OLS = lm_15_OLS_Brb_bare$residuals,IH2015)
IH2015_Brb_wResid <- IH2015_Brb_wResid %>% mutate(qn_resids_Brb = lag.listw(queens_nbrs_std, resids_OLS))

lm_15_OLS_Brb_spErr <- lm(as.formula(paste("dBIG_rightbloc ~ treat + ","qn_resids_Brb"," + ",covariates,sep = "")), IH2015_Brb_wResid, weights = pop)
summary(lm_15_OLS_Brb_spErr)
lm_15_OLS_Brb_spErr_Moran <- moran.test((cbind(resids_ = lm_15_OLS_Brb_spErr$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids_, queens_nbrs_std)$p.value

#2009 Results
summary(lm_09_OLS_Brb_bare)
lm_09_OLS_Brb_bare_Moran
summary(lm_09_OLS_Brb_wCov)
lm_09_OLS_Brb_wCov_Moran
summary(lm_09_OLS_Brb_spLag)
lm_09_OLS_Brb_spLag_Moran
summary(lm_09_OLS_Brb_spLag_wCov)
lm_09_OLS_Brb_spLag_wCov_Moran
summary(lm_09_OLS_Brb_spErr)
lm_09_OLS_Brb_spErr_Moran

#2012 Results
summary(lm_12_OLS_Brb_bare)
lm_12_OLS_Brb_bare_Moran
summary(lm_12_OLS_Brb_wCov)
lm_12_OLS_Brb_wCov_Moran
summary(lm_12_OLS_Brb_spLag)
lm_12_OLS_Brb_spLag_Moran
summary(lm_12_OLS_Brb_spLag_wCov)
lm_12_OLS_Brb_spLag_wCov_Moran
summary(lm_12_OLS_Brb_spErr)
lm_12_OLS_Brb_spErr_Moran

#2015 Results
summary(lm_15_OLS_Brb_bare)
lm_15_OLS_Brb_bare_Moran
summary(lm_15_OLS_Brb_wCov)
lm_15_OLS_Brb_wCov_Moran
summary(lm_15_OLS_Brb_spLag)
lm_15_OLS_Brb_spLag_Moran
summary(lm_15_OLS_Brb_spLag_wCov)
lm_15_OLS_Brb_spLag_wCov_Moran
summary(lm_15_OLS_Brb_spErr)
lm_15_OLS_Brb_spErr_Moran

##dlikud

#2009
lm_09_OLS_Lik_bare <- lm(as.formula(paste("dlikud ~ treat")), IH2009, weights = pop)
summary(lm_09_OLS_Lik_bare)
lm_09_OLS_Lik_bare_Moran <- moran.test((cbind(resids = lm_09_OLS_Lik_bare$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_09_OLS_Lik_wCov <- lm(as.formula(paste("dlikud ~ treat + ",covariates,sep = "")), IH2009, weights = pop)
summary(lm_09_OLS_Lik_wCov)
lm_09_OLS_Lik_wCov_Moran <- moran.test((cbind(resids = lm_09_OLS_Lik_wCov$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_09_OLS_Lik_spLag <- lm(as.formula(paste("dlikud ~ treat + ",spatial_lag_model_var,sep = "")), IH2009, weights = pop)
summary(lm_09_OLS_Lik_spLag)
lm_09_OLS_Lik_spLag_Moran <- moran.test((cbind(resids = lm_09_OLS_Lik_spLag$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_09_OLS_Lik_spLag_wCov <- lm(as.formula(paste("dlikud ~ treat + ",covariates," + ",spatial_lag_model_var,sep = "")), IH2009, weights = pop)
summary(lm_09_OLS_Lik_spLag_wCov)
lm_09_OLS_Lik_spLag_wCov_Moran <- moran.test((cbind(resids = lm_09_OLS_Lik_spLag_wCov$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value

IH2009_Lik_wResid <- cbind(resids_OLS = lm_09_OLS_Lik_bare$residuals,IH2009)
IH2009_Lik_wResid <- IH2009_Lik_wResid %>% mutate(qn_resids_Lik = lag.listw(queens_nbrs_std, resids_OLS))

lm_09_OLS_Lik_spErr <- lm(as.formula(paste("dlikud ~ treat + ","qn_resids_Lik"," + ",covariates,sep = "")), IH2009_Lik_wResid, weights = pop)
summary(lm_09_OLS_Lik_spErr)
lm_09_OLS_Lik_spErr_Moran <- moran.test((cbind(resids_ = lm_09_OLS_Lik_spErr$residuals,IH2009) %>% left_join(IH_mod %>% dplyr::filter(year == "2009") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids_, queens_nbrs_std)$p.value

#2012
lm_12_OLS_Lik_bare <- lm(as.formula(paste("dlikud ~ treat")), IH2012, weights = pop)
summary(lm_12_OLS_Lik_bare)
lm_12_OLS_Lik_bare_Moran <- moran.test((cbind(resids = lm_12_OLS_Lik_bare$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_12_OLS_Lik_wCov <- lm(as.formula(paste("dlikud ~ treat + ",covariates,sep = "")), IH2012, weights = pop)
summary(lm_12_OLS_Lik_wCov)
lm_12_OLS_Lik_wCov_Moran <- moran.test((cbind(resids = lm_12_OLS_Lik_wCov$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_12_OLS_Lik_spLag <- lm(as.formula(paste("dlikud ~ treat + ",spatial_lag_model_var,sep = "")), IH2012, weights = pop)
summary(lm_12_OLS_Lik_spLag)
lm_12_OLS_Lik_spLag_Moran <- moran.test((cbind(resids = lm_12_OLS_Lik_spLag$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_12_OLS_Lik_spLag_wCov <- lm(as.formula(paste("dlikud ~ treat + ",covariates," + ",spatial_lag_model_var,sep = "")), IH2012, weights = pop)
summary(lm_12_OLS_Lik_spLag_wCov)
lm_12_OLS_Lik_spLag_wCov_Moran <- moran.test((cbind(resids = lm_12_OLS_Lik_spLag_wCov$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value

IH2012_Lik_wResid <- cbind(resids_OLS = lm_12_OLS_Lik_bare$residuals,IH2012)
IH2012_Lik_wResid <- IH2012_Lik_wResid %>% mutate(qn_resids_Lik = lag.listw(queens_nbrs_std, resids_OLS))

lm_12_OLS_Lik_spErr <- lm(as.formula(paste("dlikud ~ treat + ","qn_resids_Lik"," + ",covariates,sep = "")), IH2012_Lik_wResid, weights = pop)
summary(lm_12_OLS_Lik_spErr)
lm_12_OLS_Lik_spErr_Moran <- moran.test((cbind(resids_ = lm_12_OLS_Lik_spErr$residuals,IH2012) %>% left_join(IH_mod %>% dplyr::filter(year == "2012") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids_, queens_nbrs_std)$p.value

#2015
lm_15_OLS_Lik_bare <- lm(as.formula(paste("dlikud ~ treat")), IH2015, weights = pop)
summary(lm_15_OLS_Lik_bare)
lm_15_OLS_Lik_bare_Moran <- moran.test((cbind(resids = lm_15_OLS_Lik_bare$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_15_OLS_Lik_wCov <- lm(as.formula(paste("dlikud ~ treat + ",covariates,sep = "")), IH2015, weights = pop)
summary(lm_15_OLS_Lik_wCov)
lm_15_OLS_Lik_wCov_Moran <- moran.test((cbind(resids = lm_15_OLS_Lik_wCov$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_15_OLS_Lik_spLag <- lm(as.formula(paste("dlikud ~ treat + ",spatial_lag_model_var,sep = "")), IH2015, weights = pop)
summary(lm_15_OLS_Lik_spLag)
lm_15_OLS_Lik_spLag_Moran <- moran.test((cbind(resids = lm_15_OLS_Lik_spLag$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value
lm_15_OLS_Lik_spLag_wCov <- lm(as.formula(paste("dlikud ~ treat + ",covariates," + ",spatial_lag_model_var,sep = "")), IH2015, weights = pop)
summary(lm_15_OLS_Lik_spLag_wCov)
lm_15_OLS_Lik_spLag_wCov_Moran <- moran.test((cbind(resids = lm_15_OLS_Lik_spLag_wCov$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids, queens_nbrs_std)$p.value

IH2015_Lik_wResid <- cbind(resids_OLS = lm_15_OLS_Lik_bare$residuals,IH2015)
IH2015_Lik_wResid <- IH2015_Lik_wResid %>% mutate(qn_resids_Lik = lag.listw(queens_nbrs_std, resids_OLS))

lm_15_OLS_Lik_spErr <- lm(as.formula(paste("dlikud ~ treat + ","qn_resids_Lik"," + ",covariates,sep = "")), IH2015_Lik_wResid, weights = pop)
summary(lm_15_OLS_Lik_spErr)
lm_15_OLS_Lik_spErr_Moran <- moran.test((cbind(resids_ = lm_15_OLS_Lik_spErr$residuals,IH2015) %>% left_join(IH_mod %>% dplyr::filter(year == "2015") %>% dplyr::select(area_num, geometry), by = c("area_num"="area_num")) %>% st_sf)$resids_, queens_nbrs_std)$p.value

#2009 Results
summary(lm_09_OLS_Lik_bare)
lm_09_OLS_Lik_bare_Moran
summary(lm_09_OLS_Lik_wCov)
lm_09_OLS_Lik_wCov_Moran
summary(lm_09_OLS_Lik_spLag)
lm_09_OLS_Lik_spLag_Moran
summary(lm_09_OLS_Lik_spLag_wCov)
lm_09_OLS_Lik_spLag_wCov_Moran
summary(lm_09_OLS_Lik_spErr)
lm_09_OLS_Lik_spErr_Moran

#2012 Results
summary(lm_12_OLS_Lik_bare)
lm_12_OLS_Lik_bare_Moran
summary(lm_12_OLS_Lik_wCov)
lm_12_OLS_Lik_wCov_Moran
summary(lm_12_OLS_Lik_spLag)
lm_12_OLS_Lik_spLag_Moran
summary(lm_12_OLS_Lik_spLag_wCov)
lm_12_OLS_Lik_spLag_wCov_Moran
summary(lm_12_OLS_Lik_spErr)
lm_12_OLS_Lik_spErr_Moran

#2015 Results
summary(lm_15_OLS_Lik_bare)
lm_15_OLS_Lik_bare_Moran
summary(lm_15_OLS_Lik_wCov)
lm_15_OLS_Lik_wCov_Moran
summary(lm_15_OLS_Lik_spLag)
lm_15_OLS_Lik_spLag_Moran
summary(lm_15_OLS_Lik_spLag_wCov)
lm_15_OLS_Lik_spLag_wCov_Moran
summary(lm_15_OLS_Lik_spErr)
lm_15_OLS_Lik_spErr_Moran

#_____________________________________________________________
###Panel Data
#_____________________________________________________________
#function to help calculate moran's i for panel data regressions: create function to take in a dataset, residuals of a regression model constructed from this dataset, and specified columns from the dataset representing an area_ID and year_ID.  It returns a dataset where each observation is an area and contains the mean residual of all years' observations of that area. This can the be fed in to a Moran's I calculation.Note that this dataset must have a geometry column called geometry.


spAutoCorInResiPrep <- function(dataForRegression, residuals_col, area_ID_col, year_col) {
  #this function takes regression results from this data, binds it to the original dataframe (containing the spatial data), calculates an average mean reisdual for each area, and returns a dataframe of mean residuals by area
  #bind together and rename residuals, area, and year columns
  tempFile <-
    cbind(residuals = residuals_col, area_num_fac = area_ID_col, year = year_col) %>%
    as.data.frame()
  #join in geometry column from original dataset
  tempFile <-
    dataForRegression %>% group_by(area_num_fac) %>% distinct(area_num_fac, .keep_all = TRUE) %>% dplyr::select(area_num_fac, geometry)%>% ungroup() %>%
    right_join(tempFile, by = c("area_num_fac"="area_num_fac")) %>%
    mutate(residuals = as.double(as.character(residuals)))
  #summarizes residuals by averaging each area's to give a mean residual score for each area
  tempFile <-
    tempFile %>%
    right_join(.,(tempFile %>% group_by(area_num_fac) %>%summarize(meanResid = mean((residuals)))), by = c("area_num_fac"="area_num_fac")) %>%
    distinct(area_num_fac,.keep_all = TRUE)
  meanResidualsByArea <- tempFile
  return(meanResidualsByArea)
}

covariates_panel <- "log(pop) + religion_pcnt_08 + africa_pcnt_08 + matriccert_pcnt_08"

###BigRightBloc
##Without fixed effects
#Using only OLS with only IsraelHayom_percent (no FE), no covariates
lm_unweighted_IsrHay_noFE_Brb_noCov_Form <- BIG_rightbloc ~ israelhayom_percent
lm_unweighted_IsrHay_noFE_Brb_noCov <- lm(lm_unweighted_IsrHay_noFE_Brb_noCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_noFE_Brb_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_noFE_Brb_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_noFE_Brb_noCov)
#Using only OLS with only IsraelHayom_percent (no FE), yes covariates
lm_unweighted_IsrHay_noFE_Brb_yesCov_Form <- as.formula(paste("BIG_rightbloc ~ israelhayom_percent + ",covariates_panel, sep=""))
lm_unweighted_IsrHay_noFE_Brb_yesCov <- lm(lm_unweighted_IsrHay_noFE_Brb_yesCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_noFE_Brb_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_noFE_Brb_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_noFE_Brb_yesCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (no FE), no covariates
lm_unweighted_spLaggedIsrHay_noFE_Brb_noCov_Form <- BIG_rightbloc ~ israelhayom_percent + qn_Israel
lm_unweighted_spLaggedIsrHay_noFE_Brb_noCov <- lm(lm_unweighted_spLaggedIsrHay_noFE_Brb_noCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_noFE_Brb_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_noFE_Brb_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_noFE_Brb_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (no FE), yes covariates
lm_unweighted_spLaggedIsrHay_noFE_Brb_yesCov_Form <- as.formula(paste("BIG_rightbloc ~ israelhayom_percent + qn_Israel + ",covariates_panel, sep=""))
lm_unweighted_spLaggedIsrHay_noFE_Brb_yesCov <- lm(lm_unweighted_spLaggedIsrHay_noFE_Brb_yesCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_noFE_Brb_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_noFE_Brb_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_noFE_Brb_yesCov)
#Preparing spatially lagged residuals from original OLS (no FE) (a true spatial error model) with and without covariates
IH_mod_w_Resid_noFE_Brb <-
  IH_mod %>%
  cbind(.,residual_bare_noCov = lm_unweighted_IsrHay_noFE_Brb_noCov$residuals,residual_bare_yesCov = lm_unweighted_IsrHay_noFE_Brb_yesCov$residuals)
IH_mod_w_Resid_noFE_Brb <-
  IH_mod_w_Resid_noFE_Brb %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 1996))$residual_bare_noCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 1999))$residual_bare_noCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2003))$residual_bare_noCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2006))$residual_bare_noCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2009))$residual_bare_noCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2013))$residual_bare_noCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2015))$residual_bare_noCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year")) %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 1996))$residual_bare_yesCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 1999))$residual_bare_yesCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2003))$residual_bare_yesCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2006))$residual_bare_yesCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2009))$residual_bare_yesCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2013))$residual_bare_yesCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Brb %>% filter(year == 2015))$residual_bare_yesCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year"))
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (no FE) (a true spatial error model), no covariates
lm_unweighted_spLaggedResids_noFE_Brb_noCov_Form <- BIG_rightbloc ~ israelhayom_percent  + qn_residual_bare_noCov
lm_unweighted_spLaggedResids_noFE_Brb_noCov <- lm(lm_unweighted_spLaggedResids_noFE_Brb_noCov_Form, data = IH_mod_w_Resid_noFE_Brb)
lm_unweighted_spLaggedResids_noFE_Brb_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_noFE_Brb,lm_unweighted_spLaggedResids_noFE_Brb_noCov$residuals,IH_mod_w_Resid_noFE_Brb$area_num_fac,IH_mod_w_Resid_noFE_Brb$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_noFE_Brb_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (no FE) (a true spatial error model), yes covariates
lm_unweighted_spLaggedResids_noFE_Brb_yesCov_Form <- as.formula(paste("BIG_rightbloc ~ israelhayom_percent  + qn_residual_bare_yesCov + ",covariates_panel, sep=""))
lm_unweighted_spLaggedResids_noFE_Brb_yesCov <- lm(lm_unweighted_spLaggedResids_noFE_Brb_yesCov_Form, data = IH_mod_w_Resid_noFE_Brb)
lm_unweighted_spLaggedResids_noFE_Brb_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_noFE_Brb,lm_unweighted_spLaggedResids_noFE_Brb_yesCov$residuals,IH_mod_w_Resid_noFE_Brb$area_num_fac,IH_mod_w_Resid_noFE_Brb$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_noFE_Brb_yesCov)
##With fixed effects
#Using only OLS with only IsraelHayom_percent (yes FE), no covariates
lm_unweighted_IsrHay_wFE_Brb_noCov_Form <- BIG_rightbloc ~ israelhayom_percent + factor(area_num) + factor(year)
lm_unweighted_IsrHay_wFE_Brb_noCov <- lm(lm_unweighted_IsrHay_wFE_Brb_noCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_wFE_Brb_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_wFE_Brb_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_wFE_Brb_noCov)
#Using only OLS with only IsraelHayom_percent (yes FE), yes covariates
lm_unweighted_IsrHay_wFE_Brb_yesCov_Form <- as.formula(paste("BIG_rightbloc ~ israelhayom_percent + factor(area_num) + factor(year) + ",covariates_panel, sep=""))
lm_unweighted_IsrHay_wFE_Brb_yesCov <- lm(lm_unweighted_IsrHay_wFE_Brb_yesCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_wFE_Brb_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_wFE_Brb_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_wFE_Brb_yesCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (yes FE), no covariates
lm_unweighted_spLaggedIsrHay_wFE_Brb_noCov_Form <- BIG_rightbloc ~ israelhayom_percent + qn_Israel + factor(area_num) + factor(year)
lm_unweighted_spLaggedIsrHay_wFE_Brb_noCov <- lm(lm_unweighted_spLaggedIsrHay_wFE_Brb_noCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_wFE_Brb_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_wFE_Brb_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_wFE_Brb_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (yes FE), yes covariates
lm_unweighted_spLaggedIsrHay_wFE_Brb_yesCov_Form <- as.formula(paste("BIG_rightbloc ~ israelhayom_percent + qn_Israel + factor(area_num) + factor(year) + ",covariates_panel, sep=""))
lm_unweighted_spLaggedIsrHay_wFE_Brb_yesCov <- lm(lm_unweighted_spLaggedIsrHay_wFE_Brb_yesCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_wFE_Brb_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_wFE_Brb_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_wFE_Brb_yesCov)
#Preparing spatially lagged residuals from original OLS (yes FE) (a true spatial error model) with and without covariates
IH_mod_w_Resid_wFE_Brb <-
  IH_mod %>%
  cbind(.,residual_bare_noCov = lm_unweighted_IsrHay_wFE_Brb_noCov$residuals,residual_bare_yesCov = lm_unweighted_IsrHay_wFE_Brb_yesCov$residuals)
IH_mod_w_Resid_wFE_Brb <-
  IH_mod_w_Resid_wFE_Brb %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 1996))$residual_bare_noCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 1999))$residual_bare_noCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2003))$residual_bare_noCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2006))$residual_bare_noCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2009))$residual_bare_noCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2013))$residual_bare_noCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2015))$residual_bare_noCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year")) %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 1996))$residual_bare_yesCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 1999))$residual_bare_yesCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2003))$residual_bare_yesCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2006))$residual_bare_yesCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2009))$residual_bare_yesCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2013))$residual_bare_yesCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Brb %>% filter(year == 2015))$residual_bare_yesCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year"))
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (yes FE) (a true spatial error model), no covariates
lm_unweighted_spLaggedResids_wFE_Brb_noCov_Form <- BIG_rightbloc ~ israelhayom_percent  + qn_residual_bare_noCov + factor(area_num) + factor(year)
lm_unweighted_spLaggedResids_wFE_Brb_noCov <- lm(lm_unweighted_spLaggedResids_wFE_Brb_noCov_Form, data = IH_mod_w_Resid_wFE_Brb)
lm_unweighted_spLaggedResids_wFE_Brb_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_wFE_Brb,lm_unweighted_spLaggedResids_wFE_Brb_noCov$residuals,IH_mod_w_Resid_wFE_Brb$area_num_fac,IH_mod_w_Resid_wFE_Brb$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_wFE_Brb_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (yes FE) (a true spatial error model), yes covariates
lm_unweighted_spLaggedResids_wFE_Brb_yesCov_Form <- as.formula(paste("BIG_rightbloc ~ israelhayom_percent  + qn_residual_bare_yesCov + factor(area_num) + factor(year) + ",covariates_panel, sep=""))
lm_unweighted_spLaggedResids_wFE_Brb_yesCov <- lm(lm_unweighted_spLaggedResids_wFE_Brb_yesCov_Form, data = IH_mod_w_Resid_wFE_Brb)
lm_unweighted_spLaggedResids_wFE_Brb_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_wFE_Brb,lm_unweighted_spLaggedResids_wFE_Brb_yesCov$residuals,IH_mod_w_Resid_wFE_Brb$area_num_fac,IH_mod_w_Resid_wFE_Brb$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_wFE_Brb_yesCov)

###Likud
##Without fixed effects
#Using only OLS with only IsraelHayom_percent (no FE), no covariates
lm_unweighted_IsrHay_noFE_Lik_noCov_Form <- Plikud ~ israelhayom_percent
lm_unweighted_IsrHay_noFE_Lik_noCov <- lm(lm_unweighted_IsrHay_noFE_Lik_noCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_noFE_Lik_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_noFE_Lik_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_noFE_Lik_noCov)
#Using only OLS with only IsraelHayom_percent (no FE), yes covariates
lm_unweighted_IsrHay_noFE_Lik_yesCov_Form <- as.formula(paste("Plikud ~ israelhayom_percent + ",covariates_panel, sep=""))
lm_unweighted_IsrHay_noFE_Lik_yesCov <- lm(lm_unweighted_IsrHay_noFE_Lik_yesCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_noFE_Lik_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_noFE_Lik_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_noFE_Lik_yesCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (no FE), no covariates
lm_unweighted_spLaggedIsrHay_noFE_Lik_noCov_Form <- Plikud ~ israelhayom_percent + qn_Israel
lm_unweighted_spLaggedIsrHay_noFE_Lik_noCov <- lm(lm_unweighted_spLaggedIsrHay_noFE_Lik_noCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_noFE_Lik_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_noFE_Lik_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_noFE_Lik_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (no FE), yes covariates
lm_unweighted_spLaggedIsrHay_noFE_Lik_yesCov_Form <- as.formula(paste("Plikud ~ israelhayom_percent + qn_Israel + ",covariates_panel, sep=""))
lm_unweighted_spLaggedIsrHay_noFE_Lik_yesCov <- lm(lm_unweighted_spLaggedIsrHay_noFE_Lik_yesCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_noFE_Lik_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_noFE_Lik_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_noFE_Lik_yesCov)
#Preparing spatially lagged residuals from original OLS (no FE) (a true spatial error model) with and without covariates
IH_mod_w_Resid_noFE_Lik <-
  IH_mod %>%
  cbind(.,residual_bare_noCov = lm_unweighted_IsrHay_noFE_Lik_noCov$residuals,residual_bare_yesCov = lm_unweighted_IsrHay_noFE_Lik_yesCov$residuals)
IH_mod_w_Resid_noFE_Lik <-
  IH_mod_w_Resid_noFE_Lik %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 1996))$residual_bare_noCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 1999))$residual_bare_noCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2003))$residual_bare_noCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2006))$residual_bare_noCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2009))$residual_bare_noCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2013))$residual_bare_noCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2015))$residual_bare_noCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year")) %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 1996))$residual_bare_yesCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 1999))$residual_bare_yesCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2003))$residual_bare_yesCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2006))$residual_bare_yesCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2009))$residual_bare_yesCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2013))$residual_bare_yesCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_noFE_Lik %>% filter(year == 2015))$residual_bare_yesCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year"))
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (no FE) (a true spatial error model), no covariates
lm_unweighted_spLaggedResids_noFE_Lik_noCov_Form <- Plikud ~ israelhayom_percent  + qn_residual_bare_noCov
lm_unweighted_spLaggedResids_noFE_Lik_noCov <- lm(lm_unweighted_spLaggedResids_noFE_Lik_noCov_Form, data = IH_mod_w_Resid_noFE_Lik)
lm_unweighted_spLaggedResids_noFE_Lik_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_noFE_Lik,lm_unweighted_spLaggedResids_noFE_Lik_noCov$residuals,IH_mod_w_Resid_noFE_Lik$area_num_fac,IH_mod_w_Resid_noFE_Lik$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_noFE_Lik_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (no FE) (a true spatial error model), yes covariates
lm_unweighted_spLaggedResids_noFE_Lik_yesCov_Form <- as.formula(paste("Plikud ~ israelhayom_percent  + qn_residual_bare_yesCov + ",covariates_panel, sep=""))
lm_unweighted_spLaggedResids_noFE_Lik_yesCov <- lm(lm_unweighted_spLaggedResids_noFE_Lik_yesCov_Form, data = IH_mod_w_Resid_noFE_Lik)
lm_unweighted_spLaggedResids_noFE_Lik_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_noFE_Lik,lm_unweighted_spLaggedResids_noFE_Lik_yesCov$residuals,IH_mod_w_Resid_noFE_Lik$area_num_fac,IH_mod_w_Resid_noFE_Lik$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_noFE_Lik_yesCov)

##With fixed effects
#Using only OLS with only IsraelHayom_percent (yes FE), no covariates
lm_unweighted_IsrHay_wFE_Lik_noCov_Form <- Plikud ~ israelhayom_percent + factor(area_num) + factor(year)
lm_unweighted_IsrHay_wFE_Lik_noCov <- lm(lm_unweighted_IsrHay_wFE_Lik_noCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_wFE_Lik_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_wFE_Lik_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_wFE_Lik_noCov)
#Using only OLS with only IsraelHayom_percent (yes FE), yes covariates
lm_unweighted_IsrHay_wFE_Lik_yesCov_Form <- as.formula(paste("Plikud ~ israelhayom_percent + factor(area_num) + factor(year) + ",covariates_panel, sep=""))
lm_unweighted_IsrHay_wFE_Lik_yesCov <- lm(lm_unweighted_IsrHay_wFE_Lik_yesCov_Form, data = IH_mod, na.action="na.exclude")
lm_unweighted_IsrHay_wFE_Lik_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_IsrHay_wFE_Lik_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_IsrHay_wFE_Lik_yesCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (yes FE), no covariates
lm_unweighted_spLaggedIsrHay_wFE_Lik_noCov_Form <- Plikud ~ israelhayom_percent + qn_Israel + factor(area_num) + factor(year)
lm_unweighted_spLaggedIsrHay_wFE_Lik_noCov <- lm(lm_unweighted_spLaggedIsrHay_wFE_Lik_noCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_wFE_Lik_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_wFE_Lik_noCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_wFE_Lik_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged values of IsraelHayom_percent (yes FE), yes covariates
lm_unweighted_spLaggedIsrHay_wFE_Lik_yesCov_Form <- as.formula(paste("Plikud ~ israelhayom_percent + qn_Israel + factor(area_num) + factor(year) + ",covariates_panel, sep=""))
lm_unweighted_spLaggedIsrHay_wFE_Lik_yesCov <- lm(lm_unweighted_spLaggedIsrHay_wFE_Lik_yesCov_Form, data = IH_mod)
lm_unweighted_spLaggedIsrHay_wFE_Lik_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod,lm_unweighted_spLaggedIsrHay_wFE_Lik_yesCov$residuals,IH_mod$area_num_fac,IH_mod$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedIsrHay_wFE_Lik_yesCov)
#Preparing spatially lagged residuals from original OLS (yes FE) (a true spatial error model) with and without covariates
IH_mod_w_Resid_wFE_Lik <-
  IH_mod %>%
  cbind(.,residual_bare_noCov = lm_unweighted_IsrHay_wFE_Lik_noCov$residuals,residual_bare_yesCov = lm_unweighted_IsrHay_wFE_Lik_yesCov$residuals)
IH_mod_w_Resid_wFE_Lik <-
  IH_mod_w_Resid_wFE_Lik %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 1996))$residual_bare_noCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 1999))$residual_bare_noCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2003))$residual_bare_noCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2006))$residual_bare_noCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2009))$residual_bare_noCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2013))$residual_bare_noCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_noCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2015))$residual_bare_noCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year")) %>%
  left_join(.,as.data.frame(rbind(cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 1996))$residual_bare_yesCov), area_num = c(1:25), year = 1996),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 1999))$residual_bare_yesCov), area_num = c(1:25), year = 1999),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2003))$residual_bare_yesCov), area_num = c(1:25), year = 2003),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2006))$residual_bare_yesCov), area_num = c(1:25), year = 2006),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2009))$residual_bare_yesCov), area_num = c(1:25), year = 2009),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2013))$residual_bare_yesCov), area_num = c(1:25), year = 2013),
                                  cbind(qn_residual_bare_yesCov = lag.listw(queens_nbrs_std, (IH_mod_w_Resid_wFE_Lik %>% filter(year == 2015))$residual_bare_yesCov), area_num = c(1:25), year = 2015))),by = c("area_num"="area_num", "year"="year"))
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (yes FE) (a true spatial error model), no covariates
lm_unweighted_spLaggedResids_wFE_Lik_noCov_Form <- Plikud ~ israelhayom_percent  + qn_residual_bare_noCov + factor(area_num) + factor(year)
lm_unweighted_spLaggedResids_wFE_Lik_noCov <- lm(lm_unweighted_spLaggedResids_wFE_Lik_noCov_Form, data = IH_mod_w_Resid_wFE_Lik)
lm_unweighted_spLaggedResids_wFE_Lik_noCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_wFE_Lik,lm_unweighted_spLaggedResids_wFE_Lik_noCov$residuals,IH_mod_w_Resid_wFE_Lik$area_num_fac,IH_mod_w_Resid_wFE_Lik$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_wFE_Lik_noCov)
#Using OLS with IsraelHayom_percent and spatially lagged residuals from original OLS (yes FE) (a true spatial error model), yes covariates
lm_unweighted_spLaggedResids_wFE_Lik_yesCov_Form <- as.formula(paste("Plikud ~ israelhayom_percent  + qn_residual_bare_yesCov + factor(area_num) + factor(year) + ",covariates_panel, sep=""))
lm_unweighted_spLaggedResids_wFE_Lik_yesCov <- lm(lm_unweighted_spLaggedResids_wFE_Lik_yesCov_Form, data = IH_mod_w_Resid_wFE_Lik)
lm_unweighted_spLaggedResids_wFE_Lik_yesCov_Moran <- moran.test(spAutoCorInResiPrep(IH_mod_w_Resid_wFE_Lik,lm_unweighted_spLaggedResids_wFE_Lik_yesCov$residuals,IH_mod_w_Resid_wFE_Lik$area_num_fac,IH_mod_w_Resid_wFE_Lik$year)$meanResid, queens_nbrs_std)$p.value
summary(lm_unweighted_spLaggedResids_wFE_Lik_yesCov)





## ----cross_section_Morans_I_p_val_table, echo=FALSE, cache=FALSE---------
cross_section_Morans_I_p_val_table <- matrix(data = c(lm_12_OLS_Lik_bare_Moran,lm_15_OLS_Lik_bare_Moran,lm_12_OLS_Brb_bare_Moran,lm_15_OLS_Brb_bare_Moran,lm_12_OLS_Lik_wCov_Moran,lm_15_OLS_Lik_wCov_Moran,lm_12_OLS_Brb_wCov_Moran,lm_15_OLS_Brb_wCov_Moran,lm_12_OLS_Lik_spLag_Moran,lm_15_OLS_Lik_spLag_Moran,lm_12_OLS_Brb_spLag_Moran,lm_15_OLS_Brb_spLag_Moran,lm_12_OLS_Lik_spLag_wCov_Moran,lm_15_OLS_Lik_spLag_wCov_Moran,lm_12_OLS_Brb_spLag_wCov_Moran,lm_15_OLS_Brb_spLag_wCov_Moran,lm_12_OLS_Lik_spErr_Moran,lm_15_OLS_Lik_spErr_Moran,lm_12_OLS_Brb_spErr_Moran,lm_15_OLS_Brb_spErr_Moran),nrow = 5, ncol = 4, byrow = TRUE, dimnames = list(list("Base","With Covariates","Spatial Lag","Spatial Lag with Covariates","Spatial Error with Covariates"), list("Likud 2013","Likud 2015","Right bloc 2013","Right bloc 2015")))
kable(cross_section_Morans_I_p_val_table, format = "html", digits = 3, padding = 1, caption = "P values of Moran's I for regressions (likelihood of error if we say there is spatial autocorrelation in model)")
SI_13 = xtable(cross_section_Morans_I_p_val_table, digits=3)
SI_13

print.xtable(SI_13, type="latex", file="Tables/Table_SI-13.tex")

## ----cross_section_Israel_Hayom_Pct_Var_effect_p_val_table, echo=FALSE, cache=FALSE----
cross_section_Israel_Hayom_Pct_Var_effect_p_val_table <- matrix(data = c(if_else(summary(lm_12_OLS_Lik_bare)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Lik_bare)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Lik_bare)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Lik_bare)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Lik_bare)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Lik_bare)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Lik_bare)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Lik_bare)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Lik_bare)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Lik_bare)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Lik_bare)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Lik_bare)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Lik_bare)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Lik_bare)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Lik_bare)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Lik_bare)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Lik_bare)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Lik_bare)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Brb_bare)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Brb_bare)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Brb_bare)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Brb_bare)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Brb_bare)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Brb_bare)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Brb_bare)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Brb_bare)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Brb_bare)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Brb_bare)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Brb_bare)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Brb_bare)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Brb_bare)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Brb_bare)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Brb_bare)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Brb_bare)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Brb_bare)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Brb_bare)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Lik_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Lik_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Lik_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Lik_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Lik_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Lik_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Lik_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Lik_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Lik_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Lik_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Lik_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Lik_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Lik_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Lik_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Lik_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Lik_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Lik_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Lik_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Brb_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Brb_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Brb_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Brb_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Brb_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Brb_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Brb_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Brb_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Brb_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Brb_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Brb_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Brb_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Brb_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Brb_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Brb_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Brb_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Brb_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Brb_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Lik_spLag)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Lik_spLag)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Lik_spLag)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Lik_spLag)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Lik_spLag)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Lik_spLag)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Lik_spLag)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Lik_spLag)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Lik_spLag)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Lik_spLag)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Lik_spLag)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Lik_spLag)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Lik_spLag)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Lik_spLag)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Lik_spLag)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Lik_spLag)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Lik_spLag)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Lik_spLag)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Brb_spLag)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Brb_spLag)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Brb_spLag)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Brb_spLag)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Brb_spLag)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Brb_spLag)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Brb_spLag)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Brb_spLag)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Brb_spLag)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Brb_spLag)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Brb_spLag)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Brb_spLag)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Brb_spLag)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Brb_spLag)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Brb_spLag)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Brb_spLag)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Brb_spLag)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Brb_spLag)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Lik_spLag_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Lik_spLag_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Brb_spLag_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Brb_spLag_wCov)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Lik_spErr)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Lik_spErr)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Lik_spErr)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Lik_spErr)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Lik_spErr)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Lik_spErr)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Lik_spErr)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Lik_spErr)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Lik_spErr)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Lik_spErr)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Lik_spErr)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Lik_spErr)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Lik_spErr)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Lik_spErr)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Lik_spErr)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Lik_spErr)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Lik_spErr)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Lik_spErr)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_12_OLS_Brb_spErr)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_12_OLS_Brb_spErr)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_12_OLS_Brb_spErr)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_12_OLS_Brb_spErr)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_12_OLS_Brb_spErr)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_12_OLS_Brb_spErr)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_12_OLS_Brb_spErr)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_12_OLS_Brb_spErr)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_12_OLS_Brb_spErr)$coefficients[2,1],3))))))
                                                                         ,if_else(summary(lm_15_OLS_Brb_spErr)$coefficients[2,4]<.001,paste(as.character(round(summary(lm_15_OLS_Brb_spErr)$coefficients[2,1],3)),"***",sep = ""),if_else(summary(lm_15_OLS_Brb_spErr)$coefficients[2,4]<.01,paste(as.character(round(summary(lm_15_OLS_Brb_spErr)$coefficients[2,1],3)),"**",sep = ""),if_else(summary(lm_15_OLS_Brb_spErr)$coefficients[2,4]<.05,paste(as.character(round(summary(lm_15_OLS_Brb_spErr)$coefficients[2,1],3)),"*",sep = ""),if_else(summary(lm_15_OLS_Brb_spErr)$coefficients[2,4]<.1,paste(as.character(round(summary(lm_15_OLS_Brb_spErr)$coefficients[2,1],3)),".",sep = ""),as.character(round(summary(lm_15_OLS_Brb_spErr)$coefficients[2,1],3))))))),
                                                                nrow = 5, ncol = 4, byrow = TRUE,
                                                                dimnames = list(list("Base","With Covariates","Spatial Lag","Spatial Lag with Covariates","Spatial Error with Covariates"),
                                                                                list("Likud 2013","Likud 2015","Right bloc 2013","Right bloc 2015")))

kable(cross_section_Israel_Hayom_Pct_Var_effect_p_val_table, format = "html", digits = 3, caption = "Magnitudes and significance of IsraelHayom Percentage variable for regressions (strength and direction of relationship between exposure to IsraelHayom Percent and dependent variable, and likelihood of this relationship being statistically significant).

      Signif. codes:  0 ‘\\*\\*\\*’ 0.001 ‘\\*\\*’ 0.01 ‘\\*’ 0.05 ‘.’ 0.1 ‘ ’ 1")

SI_14 = xtable(cross_section_Israel_Hayom_Pct_Var_effect_p_val_table)

print.xtable(SI_14, type="latex", file="Tables/Table_SI-14.tex")

## ----write_to_script, code = readLines(knitr::purl("C:/Users/alexc/Documents/Freelance/Grossman_Test/second_round_data download/appendix_cross_section_only.Rmd", documentation = 1)), echo = T, eval = F----
## NA
