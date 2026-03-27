##----------------------------------------------------------------------------##
##                   Fighting For a Better Life:
##           Protests and Public Opinion in South Africa                   
##
##              REPLICATION FILE 1: DID ANALYSIS
##----------------------------------------------------------------------------##

##----------------------------------------------------------------
## 1. Set up                                                     -
##----------------------------------------------------------------

# A function to install the required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos = c(
      CRAN = 'https://cran.rstudio.com',
      CRANextra = 'https://macos.rbind.io'
    )
    
    
    )
  sapply(pkg, require, character.only = TRUE)
}


packages_needed <- c("foreign", 
                     "readr",
                     "plyr",
                     "dplyr",
                     "tidyverse",
                     "tidyr",
                     "tibble", 
                     "knitr",
                     "printr",
                     "stargazer",
                     "ggplot2",
                     "lessR",
                     "sf",
                     "fastDummies",
                     "estimatr",
                     "fixest"
)

ipak(packages_needed)

### Set WD --------

#path = " "
#setwd(path)
#getwd()  


##----------------------------------------------------------------
## 2. Data Read In                                               -
##----------------------------------------------------------------
time_windows <- c(20, 30, 45, 60)

datasets <- lapply(time_windows, function(tw) {
  load(paste0("Rdataframe_sa_protest_DiD_", tw, ".RData"))
  get(paste0("sacs_protest_", tw))
})

names(datasets) <- paste0("tw_", time_windows)

load("Rdataframe_sa_protest_DiD_20.RData")
load("Rdataframe_sa_protest_DiD_30.RData")
load("Rdataframe_sa_protest_DiD_45.RData")
load("Rdataframe_sa_protest_DiD_60.RData")

## ------------------------------------------------------------
## 3. DiD Analysis - Main
## ------------------------------------------------------------

# Table 1 and Appendix Table A8
# ------------------------------------------------------------

# Formulas
f2 <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent
f2_covs <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent +
  female + black_coloured + age + lsm_low + metro + urban + ANC_supporter

# Function to convert geometry (lat/long)
convert_latlong <- function(df) {
  df %>%
    mutate(
      ea_center = st_centroid(geometry),
      coords = st_coordinates(ea_center),
      latitude = coords[, 2],
      longitude = coords[, 1]
    ) %>%
    dplyr::select(-coords)
}

# Function to run all models for one dataset
run_models <- function(data, f2, f2_covs, diff_parameter = 50) {
  
  # Convert geometry
  df_latlong <- convert_latlong(data)
  
  # OLS
  m_ols      <- feols(f2, df_latlong, conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))
  m_ols_cov  <- feols(f2_covs, df_latlong, conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))
  
  # Logit
  m_logit     <- femlm(f2, df_latlong, family = "logit",
                       conley(diff_parameter, distance = "spherical"),
                       fixef = c("MN_CODE", "wave"))
  m_logit_cov <- femlm(f2_covs, df_latlong, family = "logit",
                       conley(diff_parameter, distance = "spherical"),
                       fixef = c("MN_CODE", "wave"))
  
  return(list(
    ols       = m_ols,
    ols_cov   = m_ols_cov,
    logit     = m_logit,
    logit_cov = m_logit_cov
  ))
}

# Run models for all time windows via loop
all_models <- mapply(
  run_models,
  data       = datasets,
  f2         = list(f2),
  f2_covs    = list(f2_covs),
  SIMPLIFY   = FALSE
)

#  Output for latex (OLS and logit)
etable(
  all_models$tw_20$ols, all_models$tw_20$ols_cov,
  all_models$tw_30$ols, all_models$tw_30$ols_cov,
  all_models$tw_45$ols, all_models$tw_45$ols_cov,
  all_models$tw_60$ols, all_models$tw_60$ols_cov,
  tex = TRUE
)

etable(
  all_models$tw_20$logit, all_models$tw_20$logit_cov,
  all_models$tw_30$logit, all_models$tw_30$logit_cov,
  all_models$tw_45$logit, all_models$tw_45$logit_cov,
  all_models$tw_60$logit, all_models$tw_60$logit_cov,
  tex = TRUE
)

## ------------------------------------------------------------
## 4. DiD Analysis - Appendix
## ------------------------------------------------------------

# Appendix Table A1 - The Effect of Service Delivery Protest on Protest Sympathy based on buffer areas
# ------------------------------------------------------------

# Data for Robustness Check
load("Rdataframe_sa_protest_DiD_buffer_20km.RData")
load("Rdataframe_sa_protest_DiD_buffer_30km.RData")
load("Rdataframe_sa_protest_DiD_buffer_40km.RData")
load("Rdataframe_sa_protest_DiD_buffer_50km.RData")

# Defining a function to add latitude and longitude from geometry
add_latlong <- function(df) {
  df %>%
    mutate(ea_center = st_centroid(geometry)) %>%
    mutate(coords = st_coordinates(ea_center)) %>%
    mutate(
      longitude = coords[, 1],
      latitude = coords[, 2]
    ) %>%
    dplyr::select(-coords)
}

data_list <- list(
  sacs_protest_buffer_km_20 = sacs_protest_buffer_km_20,
  sacs_protest_buffer_km_30 = sacs_protest_buffer_km_30,
  sacs_protest_buffer_km_40 = sacs_protest_buffer_km_40,
  sacs_protest_buffer_km_50 = sacs_protest_buffer_km_50 
)

# Applying the function to each and overwrite the originals
data_list <- lapply(data_list, add_latlong)

# Assigning back to original names
sacs_protest_buffer_km_20 <- data_list$sacs_protest_buffer_km_20
sacs_protest_buffer_km_30 <- data_list$sacs_protest_buffer_km_30
sacs_protest_buffer_km_40 <- data_list$sacs_protest_buffer_km_40
sacs_protest_buffer_km_50 <- data_list$sacs_protest_buffer_km_50

# Setting diffusion parameter for conley std errors
diff_parameter <- 50

# Formulas
formula_protest <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent
formula_protest_cov <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent + female + black_coloured + age + lsm_low + metro + urban + ANC_supporter

# Lists for grouped models
ols_by_radius <- list()
logit_by_radius <- list()

# Looping through datasets
for (radius in names(data_list)) {
  df <- data_list[[radius]]
  
  # Fitting models
  model_ols <- feols(formula_protest, df,
                     conley(diff_parameter, distance = "spherical"),
                     fixef = c("MN_CODE", "wave"))
  
  model_ols_cov <- feols(formula_protest_cov, df,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))
  
  model_logit <- femlm(formula_protest, df,
                       family = "logit",
                       conley(diff_parameter, distance = "spherical"),
                       fixef = c("MN_CODE", "wave"))
  
  model_logit_cov <- femlm(formula_protest_cov, df,
                           family = "logit",
                           conley(diff_parameter, distance = "spherical"),
                           fixef = c("MN_CODE", "wave"))
  
  # Saving models
  ols_by_radius[[paste0(radius, " km (OLS)")]] <- model_ols
  ols_by_radius[[paste0(radius, " km (OLS + Cov)")]] <- model_ols_cov
  
  logit_by_radius[[paste0(radius, " km (Logit)")]] <- model_logit
  logit_by_radius[[paste0(radius, " km (Logit + Cov)")]] <- model_logit_cov
}

# Output for Latex
etable(ols_by_radius, tex = TRUE) # OLS
#etable(logit_by_radius, tex = TRUE) # Logit


# Appendix Figure A1-A2 - Average protest sympathy in the Days Between an Interview and Protest
# ------------------------------------------------------------

# Data for figures
load("Rdataframe_sa_protest_DiD_fig_a1_a2.RData")

# Appendix Figure A1            
# ----------------------------------------------------------------

# Dropping geometry
plotting <- st_drop_geometry(sacs_protest_200)

# Defining groups for plot
plotting2a <- plotting %>%
  mutate(protest_grp = case_when(
    protest_proximity == 1 & recent_protest == 0 ~ "Before any protest",
    protest_proximity == 1 & recent_protest == 1 & peaceful_protest == 1 ~"After a peaceful protest",
    protest_proximity == 1 & recent_protest == 1 & violent_protest == 1 ~"After a violent protest"
  ))

# Defining groups for plot
plotting2b <- plotting %>%
  mutate(protest_grp = case_when(
    protest_proximity == 1 & recent_protest == 1 & peaceful_protest == 1 ~"After a peaceful protest",
    protest_proximity == 1 & recent_protest == 1 & violent_protest == 1 ~"After a violent protest",
    protest_proximity == 1 & future_protest == 1 & peaceful_protest == 1 ~ "Before a peaceful protest",
    protest_proximity == 1 & future_protest == 1 & violent_protest == 1 ~ "Before a violent protest"
  ))

# Plotting the groups 1
plot_days_protest <- plotting2a %>%
  filter(protest_proximity==1) %>%
  group_by(protest_grp, Days_since_protest_num) %>%
  summarise(protest_symp = mean(protest_sympathy1, na.rm=TRUE),
            obs = n_distinct(NQid))
plot_days_protest2 <- plot_days_protest %>%
  ggplot(aes(x=Days_since_protest_num, y=protest_symp, colour = protest_grp)) +
  geom_smooth(aes(group = protest_grp), method = "loess") +
  geom_point() +
  geom_vline(xintercept = 0) +
  labs(x = "Days since protest", y = "Protest sympathy") +
  scale_color_discrete(name="")
plot_days_protest2


# Appendix Figure A2            
# ----------------------------------------------------------------

# Plotting the groups 2
plot_days_protestb <- plotting2b %>%
  filter(protest_proximity==1) %>%
  group_by(protest_grp, Days_since_protest_num) %>%
  summarise(protest_symp = mean(protest_sympathy1, na.rm=TRUE),
            obs = n_distinct(NQid))
plot_days_protest2b <- plot_days_protestb %>% 
  ggplot(aes(x=Days_since_protest_num, y=protest_symp, colour = protest_grp)) + 
  geom_smooth(aes(group = protest_grp), method = "loess") + 
  geom_point() + 
  geom_vline(xintercept = 0) +
  labs(x = "Days since protest", y = "Protest sympathy") +
  scale_color_discrete(name="")
plot_days_protest2b


# Appendix Table A2-A3 - The effect of the occurrence of protest events on the occurrence of survey interviews
# ------------------------------------------------------------

# Panel data for check
load("Rdataframe_sa_protest_DiD_panel_dates_x_wards.RData")

# Table A2
# -----------------------------------------------------------------

# Model with lags (past seven days) and FE
model_ols_7lags <- feols(interview ~ l(protest, 0:-7), panel_dates_x_wards,
                         panel.id=c('WardID', 'Date.x'),
                         fixef = c("WardID"))
summary(model_ols_7lags)

etable(model_ols_7lags, tex=TRUE)

# Table A3
# ----------------------------------------------------------------

# Model with lags (past seven days) and FE
model_ols_7lags_violent <- feols(interview ~ l(protest_violent, 0:-7), panel_dates_x_wards,
                                 panel.id=c('WardID', 'Date.x'),
                                 fixef = c("WardID"))
summary(model_ols_7lags_violent)

etable(model_ols_7lags_violent, tex=TRUE)


# Appendix Table A4 - Excludability check
# ------------------------------------------------------------

# Data for Excludability check
load("Rdataframe_sa_protest_DiD_excludability_20.RData")
load("Rdataframe_sa_protest_DiD_excludability_30.RData")
load("Rdataframe_sa_protest_DiD_excludability_45.RData")
load("Rdataframe_sa_protest_DiD_excludability_60.RData")


# Formulas
est_equation2 <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent
est_equation_cov2 <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent + female + black_coloured + age + lsm_low + metro + urban + ANC_supporter

### 30 days

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_robust_30 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS with covariates
model_ols_cov_30 <- feols(est_equation_cov2, sacs_protest_latlong,
                          conley(diff_parameter, distance = "spherical"),
                          fixef = c("MN_CODE", "wave"))

### 20 days                                                   

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_robust_20 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS with covariates
model_ols_cov_20 <- feols(est_equation_cov2, sacs_protest_latlong,
                          conley(diff_parameter, distance = "spherical"),
                          fixef = c("MN_CODE", "wave"))

### 45 days                                                   

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_robust_45 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS with covariates
model_ols_cov_45 <- feols(est_equation_cov2, sacs_protest_latlong,
                          conley(diff_parameter, distance = "spherical"),
                          fixef = c("MN_CODE", "wave"))

### 60 days                                                   

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_robust_60 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS with covariates
model_ols_cov_60 <- feols(est_equation_cov2, sacs_protest_latlong,
                          conley(diff_parameter, distance = "spherical"),
                          fixef = c("MN_CODE", "wave"))

# Output for Latex
etable(model_ols_cov_20,
       model_ols_cov_30,
       model_ols_cov_45,
       model_ols_cov_60,
       tex=TRUE)

# Appendix Table A5 - SUTVA check
# ------------------------------------------------------------

# Data for SUTVA check
load("Rdataframe_sa_protest_DiD_sutva_20.RData")
load("Rdataframe_sa_protest_DiD_sutva_30.RData")
load("Rdataframe_sa_protest_DiD_sutva_45.RData")
load("Rdataframe_sa_protest_DiD_sutva_60.RData")


#Formulas
f1 <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent
f1_covs <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent + female + black_coloured + age + lsm_low + metro + urban + ANC_supporter


### 30 days                                                 

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_sutva_30 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_30 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols_30cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))


### 20 days                                                 

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_sutva_20 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_20 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))


# OLS with covariates
model_ols_20cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

### 45 days                                                   

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_sutva_45 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_45 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols_45cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

### 60 days                                               

# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_sutva_60 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_60 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols_60cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

### Output for LaTeX
etable(model_ols_20cov,
       model_ols_30cov,
       model_ols_45cov,
       model_ols_60cov,
       tex=TRUE)


# Appendix Table A6 - Correlation between treatment status and item-non response
# ------------------------------------------------------------

# a) Any protest

# 20 days
sacs_protest_b <- sacs_protest_20 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest, sacs_protest_b$item_non)

test20 <- cor.test(sacs_protest_b$recent_protest, sacs_protest_b$item_non)
test20


# 30 days
sacs_protest_b <- sacs_protest_30 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest, sacs_protest_b$item_non)

test30 <- cor.test(sacs_protest_b$recent_protest, sacs_protest_b$item_non)
test30

# 45 days
sacs_protest_b <- sacs_protest_45 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest, sacs_protest_b$item_non)

test45 <- cor.test(sacs_protest_b$recent_protest, sacs_protest_b$item_non)
test45


# 60 days
sacs_protest_b <- sacs_protest_60 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest, sacs_protest_b$item_non)

test60 <- cor.test(sacs_protest_b$recent_protest, sacs_protest_b$item_non)
test60


# b) Peaceful protest

# 20 days
sacs_protest_b <- sacs_protest_20 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)

test20 <- cor.test(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)
test20


# 30 days
sacs_protest_b <- sacs_protest_30 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)

test30 <- cor.test(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)
test30

# 45 days
sacs_protest_b <- sacs_protest_45 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)

test45 <- cor.test(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)
test45

# 60 days
sacs_protest_b <- sacs_protest_60 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)

test60 <- cor.test(sacs_protest_b$recent_protest_peaceful, sacs_protest_b$item_non)
test60


# c) Violent protest

# 20 days
sacs_protest_b <- sacs_protest_20 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)

test20 <- cor.test(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)
test20


# 30 days
sacs_protest_b <- sacs_protest_30 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)

test30 <- cor.test(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)
test30

# 45 days
sacs_protest_b <- sacs_protest_45 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)

test45 <- cor.test(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)
test45


# 60 days
sacs_protest_b <- sacs_protest_60 %>%
  mutate(item_non = case_when(
    is.na(protest_sympathy1) ~ 1,
    !is.na(protest_sympathy1) ~ 0
  ))

table(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)

test60 <- cor.test(sacs_protest_b$recent_protest_violent, sacs_protest_b$item_non)
test60


# Appendix Table A7 - The effect of future service delivery protest on protest sympathy
# ------------------------------------------------------------

# Loading data for check
load("Rdataframe_sa_protest_DiD_future_20.RData")
load("Rdataframe_sa_protest_DiD_future_30.RData")
load("Rdataframe_sa_protest_DiD_future_45.RData")
load("Rdataframe_sa_protest_DiD_future_60.RData")


# Formulas for estimation
est_equation2 <-  protest_sympathy1 ~ protests_proximity + future_protest_peaceful + future_protest_violent
est_equation_cov2 <- protest_sympathy1 ~ protests_proximity + future_protest_peaceful + future_protest_violent + female + black_coloured + age + lsm_low + metro + urban + ANC_supporter

diff_parameter <- 50

### 20 days
sacs_protest_latlong <- sacs_protest_future_20 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

# OLS
model_ols2b <- feols(est_equation2, sacs_protest_latlong,
                     conley(diff_parameter, distance = "spherical"),
                     fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols2_covb <- feols(est_equation_cov2, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

# Logit regression
model_logit2b <- femlm(est_equation2, sacs_protest_latlong, 
                       family = c("logit"), 
                       conley(diff_parameter, distance = "spherical"),
                       fixef = c("MN_CODE", "wave"))

# Logit regression with covariates
model_logit2_covb <- femlm(est_equation_cov2, sacs_protest_latlong, 
                           family = c("logit"), 
                           conley(diff_parameter, distance = "spherical"),
                           fixef = c("MN_CODE", "wave"))


# Saving models per time_window for output tables
model_ols2_20 <- model_ols2b
model_ols2_cov_20 <- model_ols2_covb
model_logit2_20 <- model_logit2b
model_logit2_cov_20 <- model_logit2_covb


### 30 days
sacs_protest_latlong <- sacs_protest_future_30 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

# OLS
model_ols2b <- feols(est_equation2, sacs_protest_latlong,
                     conley(diff_parameter, distance = "spherical"),
                     fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols2_covb <- feols(est_equation_cov2, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

# Logit regression
model_logit2b <- femlm(est_equation2, sacs_protest_latlong, 
                       family = c("logit"), 
                       conley(diff_parameter, distance = "spherical"),
                       fixef = c("MN_CODE", "wave"))

# Logit regression with covariates
model_logit2_covb <- femlm(est_equation_cov2, sacs_protest_latlong, 
                           family = c("logit"), 
                           conley(diff_parameter, distance = "spherical"),
                           fixef = c("MN_CODE", "wave"))

# Saving models per time_window for output tables
model_ols2_30 <- model_ols2b
model_ols2_cov_30 <- model_ols2_covb
model_logit2_30 <- model_logit2b
model_logit2_cov_30 <- model_logit2_covb

### 45 days
sacs_protest_latlong <- sacs_protest_future_45 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

# OLS
model_ols2b <- feols(est_equation2, sacs_protest_latlong,
                     conley(diff_parameter, distance = "spherical"),
                     fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols2_covb <- feols(est_equation_cov2, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

# Logit regression
model_logit2b <- femlm(est_equation2, sacs_protest_latlong, 
                       family = c("logit"), 
                       conley(diff_parameter, distance = "spherical"),
                       fixef = c("MN_CODE", "wave"))

# Logit regression with covariates
model_logit2_covb <- femlm(est_equation_cov2, sacs_protest_latlong, 
                           family = c("logit"), 
                           conley(diff_parameter, distance = "spherical"),
                           fixef = c("MN_CODE", "wave"))

# Saving models per time_window for output tables
model_ols2_45 <- model_ols2b
model_ols2_cov_45 <- model_ols2_covb
model_logit2_45 <- model_logit2b
model_logit2_cov_45 <- model_logit2_covb

### 60 days
sacs_protest_latlong <- sacs_protest_future_60 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

# OLS
model_ols2b <- feols(est_equation2, sacs_protest_latlong,
                     conley(diff_parameter, distance = "spherical"),
                     fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols2_covb <- feols(est_equation_cov2, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

# Logit regression
model_logit2b <- femlm(est_equation2, sacs_protest_latlong, 
                       family = c("logit"), 
                       conley(diff_parameter, distance = "spherical"),
                       fixef = c("MN_CODE", "wave"))

# Logit regression with covariates
model_logit2_covb <- femlm(est_equation_cov2, sacs_protest_latlong, 
                           family = c("logit"), 
                           conley(diff_parameter, distance = "spherical"),
                           fixef = c("MN_CODE", "wave"))

# Saving models per time_window for output tables
model_ols2_60 <- model_ols2b
model_ols2_cov_60 <- model_ols2_covb
model_logit2_60 <- model_logit2b
model_logit2_cov_60 <- model_logit2_covb

### Output for LaTeX
etable(model_ols2_20, 
       model_ols2_cov_20, 
       model_ols2_30, 
       model_ols2_cov_30, 
       model_ols2_45, 
       model_ols2_cov_45, 
       model_ols2_60, 
       model_ols2_cov_60,
       tex=TRUE)

etable(model_logit2_20, 
       model_logit2_cov_20, 
       model_logit2_30, 
       model_logit2_cov_30, 
       model_logit2_45, 
       model_logit2_cov_45, 
       model_logit2_60, 
       model_logit2_cov_60, 
       tex=TRUE)


# Appendix Table A9 - The effect of service delivery protest on protest sympathy - Alternative definition of violent protest
# ------------------------------------------------------------

# Loading data for check of alternative definition of violent protest
load("Rdataframe_sa_protest_DiD_adj_violent_20.RData")
load("Rdataframe_sa_protest_DiD_adj_violent_30.RData")
load("Rdataframe_sa_protest_DiD_adj_violent_45.RData")
load("Rdataframe_sa_protest_DiD_adj_violent_60.RData")

sacs_protest_20 <- sacs_protest_adj_violent_20
sacs_protest_30 <- sacs_protest_adj_violent_30
sacs_protest_45 <- sacs_protest_adj_violent_45
sacs_protest_60 <- sacs_protest_adj_violent_60

#Formulas
f1 <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent
f1_covs <- protest_sympathy1 ~ protests_proximity + recent_protest_peaceful + recent_protest_violent + female + black_coloured + age + lsm_low + metro + urban + ANC_supporter

### 30 days                                                   -
# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_30 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_30 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols_30cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

### 20 days                                                   
# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_20 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_20 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols_20cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

### 45 days                                                   
# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_45 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_45 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols_45cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

### 60 days                                                  
# Converting geometry to Lat/Long
sacs_protest_latlong <- sacs_protest_60 %>% 
  mutate(
    ea_center = st_centroid(geometry), # Computing centroid of EA polygon
    ea_center2 = gsub('[POINT ()]', '', ea_center), # removing "POINT" and paratheses from string
    ea_center3 = gsub('c', '', ea_center2)) %>% # removing c from string
  separate(col = ea_center3, into = c('Lat_chr', 'Lon_chr'), sep = '\\,') %>% # seperating into two different variables
  mutate(latitude = as.numeric(Lat_chr), # converting to numeric
         longitude = as.numeric(Lon_chr)) %>%
  dplyr::select(-ea_center2, -Lat_chr, -Lon_chr) # removing temp variables

diff_parameter <- 50

# OLS
model_ols_60 <- feols(f1, sacs_protest_latlong,
                      conley(diff_parameter, distance = "spherical"),
                      fixef = c("MN_CODE", "wave"))

# OLS with covariates
model_ols_60cov <- feols(f1_covs, sacs_protest_latlong,
                         conley(diff_parameter, distance = "spherical"),
                         fixef = c("MN_CODE", "wave"))

### Output for LaTeX
etable(model_ols_20cov,
       model_ols_30cov,
       model_ols_45cov,
       model_ols_60cov,
       tex=TRUE)



##----------------------------------------------------------------------------##
##                            END OF SCRIPT 
##----------------------------------------------------------------------------##



