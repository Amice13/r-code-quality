#Replication Data for: Stengel,Tristan (2024): The Impact of Elite Cues on Public Opinion on Climate Change. Mediation through Media Coverage

#Download Data from https://doi.org/10.7910/DVN/NM74AU

#Required Packages ----
library(tidyverse)
library(lubridate)
library(lavaan)

setwd("C:/Tris/Master Political Science/MA/Replication") #Adjust Accordingly
setwd("C:/Replication/") #Adjust Accordingly

##Explanation of variables ----
colnames(final_data)
#date: Last day of the fieldwork day for the Standard Eurobarometer Series of the respective country.
#country: unique character identifier for each country. UK = United Kingdom, GER = Germany, HU = Hungary.
#cc_env_2: % of respondents per country concerned about climate change, the environment, and energy issues (Eurobarometer item).
#avg_cc_cue: average mentions of climate change, global warming, and CO2 in the 180-/90-/30-day period prior to each Eurobarometer survey.
#sum_cc_media: sum of articles on climate change and global warming in the 180-/90-/30-day period prior to each Eurobarometer survey.
#sum_cc_google: sum of Google Trends score on the "topic" climate change in the 180-/90-/30-day period prior to each Eurobarometer survey.
#avg_GDP_defl: average GDP deflator for the 180-/90-day period prior to each Eurobarometer wave. Available quarterly, hence no 30-day measurement possible.
#avg_unemploy: average seasonally adjusted unemployment rate for the 180-/90-day period prior to each Eurobarometer wave. Available quarterly, hence no 30-day measurement possible.
#avg_drought_perc: average % of country affected by drought in the 180-/90-/30-day period prior to each Eurobarometer wave. Based on the scPDSI. Threshold defined as <10%.
#avg_precip_perc: average % of country affected by extreme one-day precipitation in the 180-/90-/30-day period prior to each Eurobarometer wave. Based on total daily precipitation.Threshold defined as >90%. 
#avg_temp_perc: average % of country affected by extremely high temperatures in the 180-/90-/30-day period prior to each Eurobarometer wave. Based on maximum daily temperature. Threshold defined as >90%. 

#The sources can be found in the bibliography of the manuscript.

#180-Day Operationalization: Data imports and logs ----


final_data<- read.csv("final_data.csv")

data_UK <- final_data %>%
  filter(country =="UK")


data_GER <- final_data %>%
  filter(country =="GER")

data_GER_log <- data_GER %>%  #Logarithmic transformation
  mutate(
    cc_env_2 = log(cc_env_2),
    sum_cc_media = log(sum_cc_media),
    sum_cc_google = log(sum_cc_google),
    avg_cc_cue = log(avg_cc_cue),
    avg_GDP_defl = log(avg_GDP_defl),
    avg_unemploy = log(avg_unemploy),
  )
offset <- 1 #Extreme Weather with offset. I set offset to 1, because log(1)=0
data_GER_log$avg_drought_perc <- log(data_GER$avg_drought_perc + offset)
data_GER_log$avg_precip_perc <- log(data_GER$avg_precip_perc + offset)
data_GER_log$avg_temp_perc <- log(data_GER$avg_temp_perc + offset)

data_GER_1_log <- data_GER_log %>%
  filter(country =="GER" & date < "2013-01-01")
data_GER_2_log <- data_GER_log %>%
  filter(country =="GER" & date >= "2013-01-01")

data_HU <- final_data %>%
  filter(country =="HU")

data_HU_2 <- data_HU %>%
  filter(date >= "2010-06-01")

##Main Models ----

#UK
model3.1_UK <- '
              # Messmodel
              ExtremeWeather =~ avg_drought_perc + avg_precip_perc + avg_temp_perc
              # Strukturmodell/Regression
              #sum_cc_media ~ avg_cc_cue + avg_GDP_defl + avg_unemploy + ExtremeWeather
              #cc_env_2 ~ sum_cc_media
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy + f*ExtremeWeather
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ ExtremeWeather
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_weath := g*f
              '
model_fit_UK <- sem(data = data_UK, model = model3.1_UK, std.ov=TRUE, std.lv=TRUE)
summary(model_fit_UK, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Germany
model2.1.2_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc + h*avg_precip_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ a*avg_cc_cue
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_precip_perc
              cc_env_2 ~ avg_temp_perc
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              ind_precip := g*h
              '
model_fit_GER <- sem(data = data_GER_log, model = model2.1.2_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Hungary
model4.10_HU <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc + h*avg_precip_perc + k*avg_temp_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_temp_perc
              avg_cc_cue ~ avg_unemploy
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              ind_temp := g*k
              ind_precip := g*h
              '
model_fit_HU <- sem(data = data_HU, model = model4.10_HU)
summary(model_fit_HU, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Robustness checks ----

##180days ----

#UK with alternative operationalization: Google Trends instead of the Print Media

model5.1_UK <- '
              #Strukturmodell/Regression
              sum_cc_google ~ avg_cc_cue + avg_GDP_defl + avg_unemploy + avg_drought_perc + avg_precip_perc + avg_temp_perc
              cc_env_2 ~ sum_cc_google
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_precip_perc
              '
model_fit_UK <- sem(data = data_UK, model = model5.1_UK)
summary(model_fit_UK, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Germany pre-2013
model2.1.2_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_precip_perc
              cc_env_2 ~ avg_temp_perc
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              '
model_fit_GER <- sem(data = data_GER_1_log, model = model2.1.2_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Germany post-2013
model2.1.2_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_precip_perc
              cc_env_2 ~ avg_temp_perc
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              '
model_fit_GER <- sem(data = data_GER_2_log, model = model2.1.2_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Hungary post-2010
model4.9_HU <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc + h*avg_precip_perc + k*avg_temp_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_GDP_defl
              avg_cc_cue ~ avg_unemploy
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              ind_temp := g*k
              ind_precip := g*h
              '
model_fit_HU <- sem(data = data_HU_2, model = model4.9_HU)
summary(model_fit_HU, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#data sets are named identically and should be overwritten. Clear environment to be safe.


#90-Day Operationalization: Data imports and logs ----
final_data_90d<- read.csv("90d_final_data.csv")

data_UK <- final_data_90d %>%
  filter(country =="UK")

data_GER <- final_data_90d %>%
  filter(country =="GER")

data_GER_log <- data_GER %>% 
  mutate(
    cc_env_2 = log(cc_env_2),
    sum_cc_media = log(sum_cc_media),
    sum_cc_google = log(sum_cc_google),
    avg_cc_cue = log(avg_cc_cue),
    avg_GDP_defl = log(avg_GDP_defl),
    avg_unemploy = log(avg_unemploy),
  )
offset <- 1 #Extreme Weather with offset. I set offset to 1, because log(1)=0
data_GER_log$avg_drought_perc <- log(data_GER$avg_drought_perc + offset)
data_GER_log$avg_precip_perc <- log(data_GER$avg_precip_perc + offset)
data_GER_log$avg_temp_perc <- log(data_GER$avg_temp_perc + offset)

data_GER_1_log <- data_GER_log %>%
  filter(date < "2013-01-01")
data_GER_2_log <- data_GER_log %>%
  filter(date >= "2013-01-01")

data_HU <- final_data_90d %>%
  filter(country =="HU")

data_HU_2 <- data_HU %>%
  filter(date >= "2010-06-01")

##Main Models ----

#UK
model3.3_UK <- '
              #Messmodel
              ExtremeWeather =~ avg_drought_perc  + avg_temp_perc 
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + e*avg_unemploy + d*avg_GDP_defl + c*ExtremeWeather
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_precip_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_weath := g*c
              '
model_fit_UK <- sem(data = data_UK, model = model3.3_UK)
summary(model_fit_UK, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Germany
model2_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue  + d*avg_GDP_defl
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_precip_perc
              cc_env_2 ~ avg_temp_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              '
model_fit_GER <- sem(data = data_GER, model = model2_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Hungary
model2_HU <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + e*avg_unemploy  + d*avg_GDP_defl   
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_temp_perc
              cc_env_2 ~ avg_precip_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              '
model_fit_HU <- sem(data = data_HU, model = model2_HU)
summary(model_fit_HU, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


##Subset Models ----

#Germany pre-2013 (LOGGED!)
model2.2_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue  + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc  + h*avg_precip_perc + k*avg_temp_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_drought_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_weath := g*c
              ind_drought := g*i
              ind_temp := g*k
              ind_precip := g*h  
              '
model_fit_GER <- sem(data = data_GER_1_log, model = model2.2_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
#df=2; CFI=0.997; RMSEA=0.060; SRMR=0.012; chi n.s. => Good fit

#Germany post-2013 (LOGGED!)
model2.3_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue  + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc  + h*avg_precip_perc + k*avg_temp_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_temp_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_weath := g*c
              ind_drought := g*i
              ind_temp := g*k
              ind_precip := g*h  
              '
model_fit_GER <- sem(data = data_GER_2_log, model = model2.3_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Hungary post-2010
model2.1_HU <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + e*avg_unemploy 
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_temp_perc
              cc_env_2 ~ avg_precip_perc
              # Mediation
              ind_cue := g*c
              ind_unempl := g*e
              '
model_fit_HU <- sem(data = data_HU_2, model = model2.1_HU)
summary(model_fit_HU, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#30-Day Operationalization: Data imports ----

final_data_30d <- read_csv("30d_final_data.csv")

data_UK <- final_data_30d %>%
  filter(country =="UK")

data_GER <- final_data_30d %>%
  filter(country =="GER")
data_GER_1 <- data_GER %>%
  filter(date < "2013-01-01")
data_GER_2 <- data_GER %>%
  filter(date >= "2013-01-01")

data_HU <- final_data_30d %>%
  filter(country =="HU")
data_HU_2 <- data_HU %>%
  filter(date >= "2010-06-01")


##Main Models ----

#UK
model2.1.4_UK <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_drought_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              '
model_fit_UK <- sem(data = data_UK, model = model2.1.4_UK)
summary(model_fit_UK, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Germany
model2.1.2_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + d*avg_GDP_defl + e*avg_unemploy + i*avg_drought_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_precip_perc
              cc_env_2 ~ avg_temp_perc
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              '
model_fit_GER <- sem(data = data_GER, model = model2.1.2_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Hungary
model1.3_HU <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + e*avg_unemploy + d*avg_GDP_defl + i*avg_drought_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_temp_perc
              cc_env_2 ~ avg_precip_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              '
model_fit_HU <- sem(data = data_HU, model = model1.3_HU)
summary(model_fit_HU, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


##Subset Models ----

#Germany pre-2013
modelX3_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + i*avg_drought_perc + h*avg_precip_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_precip_perc
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_temp_perc
              # Mediation
              ind_cue := g*c
              ind_drought := g*i
              ind_precip := g*h
              '
model_fit_GER <- sem(data = data_GER_1, model = modelX3_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Germany post-2013
modelX1_GER <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + i*avg_drought_perc + h*avg_precip_perc   
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_precip_perc
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_temp_perc
              # Mediation
              ind_cue := g*c
              ind_drought := g*i
              ind_precip := g*h
              '
model_fit_GER <- sem(data = data_GER_2, model = modelX1_GER)
summary(model_fit_GER, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Hungary

#30day. HU post 2010
model1.5_HU <- '
              #Strukturmodell/Regression
              sum_cc_media ~ c*avg_cc_cue + e*avg_unemploy + d*avg_GDP_defl + i*avg_drought_perc
              cc_env_2 ~ g*sum_cc_media
              cc_env_2 ~ avg_cc_cue
              cc_env_2 ~ avg_unemploy
              cc_env_2 ~ avg_GDP_defl
              cc_env_2 ~ avg_drought_perc
              cc_env_2 ~ avg_precip_perc
              # Mediation
              ind_cue := g*c
              ind_GDP := g*d
              ind_unempl := g*e
              ind_drought := g*i
              '
model_fit_HU <- sem(data = data_HU_2, model = model1.5_HU)
summary(model_fit_HU, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#fin.