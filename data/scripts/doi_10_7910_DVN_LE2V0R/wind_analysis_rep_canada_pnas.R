########################################################################################################
#### Replication for: Prevalence and predictors of wind energy opposition in North America (Canada) ####
########################################################################################################

# Set Working Directory #

# Load Packages #

pacman::p_load(readxl, ggplot2, ggmap, maps, mapdata, magrittr, 
               gridExtra, grid, xtable, stargazer, dplyr, DescTools, plm, lmtest)

# Set Options #

options(max.print = 100000)

`%notin%` <- Negate(`%in%`)

#### Load Data ####  

wind_data_canada <- read.csv("wind_data_canada.csv")

#### Summary Statistics ####

# Percent of Projects By Region #

wind_data_canada %>%
  dplyr::mutate(n_projs = n()) %>%
  group_by(prov_ter_6) %>%
  dplyr::summarise(region_perc = n() / n_projs) %>%
  slice(1)

# Percent of Projects With Opposition By Region #

wind_data_canada %>%
  filter(is_anti_wind == 1) %>%
  dplyr::mutate(n_projs = n()) %>%
  group_by(prov_ter_6 ) %>%
  dplyr::summarise(anti_perc = n() / n_projs) %>%
  slice(1)

# Percent with Protest #

round(mean(wind_data_canada$is_anti_wind, na.rm = T) * 100, 1) 

# Percent Without Protest #

100 - round(mean(wind_data_canada$is_anti_wind, na.rm = T) * 100, 1)

# Percent of Projects with Opposition 2000-2008 #
round(mean(wind_data_canada$is_anti_wind[which(wind_data_canada$year <= 2008)], na.rm = T) * 100, 1)

# Percent of Projects with Opposition 2009-2016 #
round(mean(wind_data_canada$is_anti_wind[which(wind_data_canada$year > 2008)], na.rm = T) * 100, 1)


# Table of Protests #

table(wind_data_canada$is_anti_wind)

table(wind_data_canada$is_anti_wind[which(wind_data_canada$year <= 2008)])
table(wind_data_canada$is_anti_wind[which(wind_data_canada$year > 2008)])


#### Tables ####

# Descriptive Characteristics Table (Table 1) # 

descrip_tab_can <- t(aggregate(wind_data_canada[ , c("pop_den", "med_inc", "white_perc", "lib_share", "total_mw", "n_turbs")],
                               list(wind_data_canada$is_anti_wind),
                               function(x)
                                 mean(x, na.rm = T)))
round(descrip_tab_can, 2)


# T-tests On Relevant Variables #

# Demographics #
t.test(wind_data_canada$pop_den[wind_data_canada$is_anti_wind == 1], 
       wind_data_canada$pop_den[wind_data_canada$is_anti_wind == 0])

t.test(wind_data_canada$med_inc[wind_data_canada$is_anti_wind == 1], 
       wind_data_canada$med_inc[wind_data_canada$is_anti_wind == 0])

t.test(wind_data_canada$white_perc[wind_data_canada$is_anti_wind == 1], 
       wind_data_canada$white_perc[wind_data_canada$is_anti_wind == 0])

t.test(wind_data_canada$lib_share[wind_data_canada$is_anti_wind == 1], 
       wind_data_canada$lib_share[wind_data_canada$is_anti_wind == 0])

t.test(wind_data_canada$total_mw[wind_data_canada$is_anti_wind == 1], 
       wind_data_canada$total_mw[wind_data_canada$is_anti_wind == 0])

t.test(wind_data_canada$n_turbs[wind_data_canada$is_anti_wind == 1], 
       wind_data_canada$n_turbs[wind_data_canada$is_anti_wind == 0])


# Opposition Tactics (Table 2) # 

# Percent Using Letters #

mean(wind_data_canada$letters_use[wind_data_canada$is_anti_wind == 1], na.rm = T)

# Percent Using Courts #

mean(wind_data_canada$courts_use[wind_data_canada$is_anti_wind == 1], na.rm = T)

# Percent Using Legislation #

mean(wind_data_canada$legislation_use[wind_data_canada$is_anti_wind == 1], na.rm = T)

# Percent Recording Protesters #
wind_data_canada$protesters_bin <- ifelse(wind_data_canada$protesters_mean >= 1, 1, 0)
wind_data_canada$protesters_bin[is.na(wind_data_canada$protesters_mean)] <- 0

length(which(wind_data_canada$protesters_bin[wind_data_canada$is_anti_wind == 1] >= 1)) / nrow(wind_data_canada[which(wind_data_canada$is_anti_wind == 1), ])

# Mean Number Of Protesters #

mean(wind_data_canada$protesters_mean[wind_data_canada$is_anti_wind == 1 & wind_data_canada$protesters_mean > 0], na.rm = T) # 31 projects had num protesters 

# Median Number of Protesters #
median(wind_data_canada$protesters_mean[wind_data_canada$is_anti_wind == 1 & wind_data_canada$protesters_mean > 0], na.rm = T)


# Continuous DV Table (Table 3) # 
# Create Continuous DV # 

wind_data_canada$courts_3 <- ifelse(wind_data_canada$courts_use >= 1 & wind_data_canada$is_anti_wind == 1, 1, 0)
wind_data_canada$legis_3 <- ifelse(wind_data_canada$legislation_use >= 1 & wind_data_canada$is_anti_wind == 1, 1, 0)
wind_data_canada$letters_3 <- ifelse(wind_data_canada$letters_use >= 1 & wind_data_canada$is_anti_wind == 1, 1, 0)
wind_data_canada$prots_3 <- ifelse(wind_data_canada$protesters_bin > 0 & wind_data_canada$is_anti_wind == 1, 1, 0)
wind_data_canada$prots_3 <- ifelse(is.na(wind_data_canada$prots_3), 0, wind_data_canada$prots_3)
wind_data_canada$anti_wind_cont <- wind_data_canada$courts_3 + wind_data_canada$legis_3 + wind_data_canada$letters_3  + wind_data_canada$prots_3

cont_wind_tab <- round(t(aggregate(wind_data_canada[which(wind_data_canada$is_anti_wind == 1), 
                                                  c("pop_den", "med_inc", "white_perc", "lib_share", "total_mw", "n_turbs")],
                                   by = list(wind_data_canada$anti_wind_cont[wind_data_canada$is_anti_wind == 1]),
                                   function(x) 
                                     mean(x, na.rm = T))), 2)

round(cont_wind_tab, 2)

# Continuous DV Correlation (Table 3 (r))#

apply(wind_data_canada[which(wind_data_canada$is_anti_wind == 1), 
                     c("pop_den", "med_inc", "white_perc", "lib_share", "total_mw", "n_turbs")], 
      2, 
      function(x) 
        cor.test(x, wind_data_canada$anti_wind_cont[wind_data_canada$is_anti_wind == 1]))

# Count Prevalence of Opposition Tactics #

wind_data_canada %>%
  filter(is_anti_wind == 1) %>%
  dplyr::count(anti_wind_cont)


#### Models ####

# OLS Models #

# OLS # - Plant Characteristics

mod_1_ols_can <- lm(is_anti_wind ~ # DV = dummy variable indicating presence of anti-wind protest.
                      I(pop_den / 1000) + 
                      I(med_inc / 10000) + 
                      white_perc, 
                    data = wind_data_canada)

summary(mod_1_ols_can)

# OLS # - Plant and SES Characteristics

mod_2_ols_can <- lm(is_anti_wind ~ # DV = dummy variable indicating presence of anti-wind protest.
                      I(pop_den / 1000) + 
                      I(med_inc / 10000) + 
                      white_perc +
                      total_mw + 
                      n_turbs,
                    data = wind_data_canada)

summary(mod_2_ols_can)

# OLS # - Plant, SES, and Political Characteristics

mod_3_ols_can <- lm(is_anti_wind ~ # DV = dummy variable indicating presence of anti-wind protest.
                      I(pop_den / 1000) + 
                      I(med_inc / 10000) + 
                      white_perc +
                      total_mw + 
                      n_turbs + 
                      prov_ter_6 + 
                      lib_share, 
                    data = wind_data_canada)

summary(mod_3_ols_can)

# Create Covariate List #

mod_covariates_can <- c("Population Density (mi^2, per 1,000)", "Median Income (per 10,000)",
                        "Percent White",  "Total Megawatts",  "Number of Turbines",
                        "British Columbia", "Canadian Prairies", "Ontario", "Quebec", "Percent Liberal")



# OLS Models Table #

stargazer(mod_1_ols_can, mod_2_ols_can, mod_3_ols_can,
          type = "latex",
          covariate.labels = mod_covariates_can,
          column.sep.width = "-15pt")

# OLS Models Table with Clustered Standard Errors#

mod_ols_list_can <- list(mod_1_ols_can, mod_2_ols_can, mod_3_ols_can)

se_cluster_can <- function(x)
  coeftest(x, vcov. = vcovHC, cluster = ~csduid)[, "Std. Error"]

stargazer(mod_ols_list_can,
          type = "latex",
          covariate.labels = mod_covariates_can,
          column.sep.width = "-15pt",
          se = lapply(mod_ols_list_can, se_cluster_can))

