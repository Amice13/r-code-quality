########################################################################################################
#### Replication for: Prevalence and predictors of wind energy opposition in North America (USA) #######
########################################################################################################

# Set Working Directory #

# Load Packages #

pacman::p_load(readxl, ggplot2, ggmap, maps, mapdata, magrittr, 
               gridExtra, grid, xtable, stargazer, dplyr, DescTools, plm, lmtest)

# Set Options #

options(max.print = 100000)

`%notin%` <- Negate(`%in%`)

#### Load Data ####  

wind_data_usa <- read.csv("wind_data_usa.csv")

#### Summary Statistics ####

# Percent of Projects By Region #

wind_data_usa %>%
  dplyr::mutate(n_projs = n()) %>%
  group_by(region_meta) %>%
  dplyr::summarise(region_perc = n() / n_projs) %>%
  slice(1)

# Percent of Projects With Opposition By Region #

wind_data_usa %>%
  dplyr::filter(is_anti_wind == 1) %>%
  dplyr::mutate(n_projs = n()) %>%
  group_by(region_meta) %>%
  dplyr::summarise(anti_perc = n() / n_projs) %>%
  slice(1)

# Percent With and Without Protest By Region #

wind_data_usa %>%
  group_by(region_meta) %>%
  dplyr::mutate(n_projs = n()) %>%
  group_by(region_meta, is_anti_wind) %>%
  dplyr::summarise(anti_perc = n() / n_projs) %>%
  slice(1)

# Percent Of Projects with Protest #

round(mean(wind_data_usa$is_anti_wind, na.rm = T) * 100, 1)

# Percent of Projects Without Protest #

100 - round(mean(wind_data_usa$is_anti_wind, na.rm = T) * 100, 1)

# Percent of Projects with Opposition 2000-2008 #
round(mean(wind_data_usa$is_anti_wind[which(wind_data_usa$year <= 2008)], na.rm = T) * 100, 1)

# Percent of Projects with Opposition 2009-2016 #
round(mean(wind_data_usa$is_anti_wind[which(wind_data_usa$year > 2008)], na.rm = T) * 100, 1)

# Percent of Projects with Community Ownership #
round(mean(wind_data_usa$community_ownership, na.rm = T) * 100, 1)

# Table of Protests #

table(wind_data_usa$is_anti_wind)

table(wind_data_usa$is_anti_wind[which(wind_data_usa$year <= 2008)])
table(wind_data_usa$is_anti_wind[which(wind_data_usa$year > 2008)])

#### Tables ####

# Descriptive Characteristics Table (Table 1) # 

descrip_tab <- t(aggregate(wind_data_usa[ , c("pop_den", "med_inc", "white_perc", "his_lat_perc", "black_perc" ,"community_ownership", "gop_per", "total_mw", "n_turbs", "turb_ht")],
                           list(wind_data_usa$is_anti_wind),
                           function(x) 
                             mean(x, na.rm = T)))

round(descrip_tab,2)

# T-tests On Relevant Variables #
# Demographics #

t.test(wind_data_usa$pop_den[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$pop_den[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$med_inc[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$med_inc[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$white_perc[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$white_perc[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$his_lat_perc[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$his_lat_perc[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$black_perc[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$black_perc[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$community_ownership[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$community_ownership[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$gop_per[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$gop_per[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$total_mw[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$total_mw[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$n_turbs[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$n_turbs[wind_data_usa$is_anti_wind == 0])

t.test(wind_data_usa$turb_ht[wind_data_usa$is_anti_wind == 1], 
       wind_data_usa$turb_ht[wind_data_usa$is_anti_wind == 0])

# Opposition Tactics (Table 2) # 
# Percent Using Letters #

length(which(wind_data_usa$letters_use[wind_data_usa$is_anti_wind == 1] >= 1)) / nrow(wind_data_usa[which(wind_data_usa$is_anti_wind == 1), ])

# Percent Using Courts #

length(which(wind_data_usa$courts_use[wind_data_usa$is_anti_wind == 1] >= 1)) / nrow(wind_data_usa[which(wind_data_usa$is_anti_wind == 1), ])

# Percent Using Legislation #

length(which(wind_data_usa$legislation_use[wind_data_usa$is_anti_wind == 1] >= 1)) / nrow(wind_data_usa[which(wind_data_usa$is_anti_wind == 1), ])

# Percent Recording Protesters #
wind_data_usa$protesters_bin <- ifelse(wind_data_usa$protesters_mean >= 1, 1, 0)
wind_data_usa$protesters_bin[is.na(wind_data_usa$protesters_mean)] <- 0

length(which(wind_data_usa$protesters_bin[wind_data_usa$is_anti_wind == 1] >= 1)) / nrow(wind_data_usa[which(wind_data_usa$is_anti_wind == 1), ])

# Mean Number Of Protesters #

mean(wind_data_usa$protesters_mean[wind_data_usa$is_anti_wind == 1 & wind_data_usa$protesters_mean > 0], na.rm = T)  # 68 projects had mean num participants

# Median Number of Protesters #

median(wind_data_usa$protesters_mean[wind_data_usa$is_anti_wind == 1 & wind_data_usa$protesters_mean > 0], na.rm = T) 


# Continuous DV Table (Table 3) # 
# Create Continuous DV # 

wind_data_usa <- wind_data_usa %>%
  mutate(courts_3 = if_else(courts_use >= 1 & is_anti_wind == 1, 1, 0),
         legis_3 = if_else(legislation_use >= 1 & is_anti_wind == 1, 1, 0),
         letters_3 = if_else(letters_use >= 1 & is_anti_wind == 1, 1, 0),
         prots_3 = if_else(protesters_bin >= 1 & is_anti_wind == 1, 1, 0),
         anti_wind_cont = courts_3 + legis_3 + letters_3 + prots_3)

# Table 3 #

cont_wind_tab <- t(aggregate(wind_data_usa[which(wind_data_usa$is_anti_wind == 1), 
                                                c("pop_den", "med_inc", "white_perc", "his_lat_perc","black_perc" ,"gop_per", "community_ownership", "total_mw", "n_turbs", "turb_ht")],
                             by = list(wind_data_usa$anti_wind_cont[wind_data_usa$is_anti_wind == 1]),
                             function(x) 
                               mean(x, na.rm = T)))

round(cont_wind_tab, 2)

# Count Prevalence of Opposition Tactics #

wind_data_usa %>%
  filter(is_anti_wind == 1) %>%
  dplyr::count(anti_wind_cont)

# Correlations (Table 3 (r)) #

wind_sub <- wind_data_usa[which(wind_data_usa$is_anti_wind == 1), ]

apply(wind_sub[ , c("pop_den", "med_inc", "white_perc", "his_lat_perc", "black_perc" ,"gop_per", "community_ownership", "total_mw", "n_turbs", "turb_ht")], 
      2, 
      function(x) 
        cor.test(x, wind_sub$anti_wind_cont))

#### Models ####

# OLS Models #

# IVs: Demographic characteristics #

mod_1_ols <- lm(is_anti_wind ~ # DV = dummy variable indicating presence of anti-wind protest.
                  I(pop_den / 1000) + 
                  I(med_inc / 10000) + 
                  white_perc + 
                  his_lat_perc +
                  black_perc +
                  as.factor(community_ownership),  
                data = wind_data_usa)

summary(mod_1_ols)

coeftest(mod_1_ols, vcov. = vcovHC, cluster = ~cen_tract_id)

# IVs: Demographics + Wind Plant Characteristics #

mod_2_ols <- lm(is_anti_wind ~ # DV = dummy variable indicating presence of anti-wind protest.
                  I(pop_den / 1000) + 
                  I(med_inc / 10000) + 
                  white_perc + 
                  his_lat_perc +
                  black_perc +
                  as.factor(community_ownership) +
                  total_mw + 
                  n_turbs + 
                  turb_ht,
                data = wind_data_usa)

summary(mod_2_ols)

coeftest(mod_2_ols, vcov. = vcovHC, cluster = ~cen_tract_id)


# IVs: Demographics + Wind Plant Characteristics + Political + Region + Comm Own #

mod_3_ols <- lm(is_anti_wind ~ # DV = dummy variable indicating presence of anti-wind protest.
                  I(pop_den / 1000) + 
                  I(med_inc / 10000) + 
                  white_perc + 
                  his_lat_perc +
                  black_perc +
                  as.factor(community_ownership) +
                  total_mw + 
                  n_turbs + 
                  turb_ht +
                  region_meta +
                  gop_per,
                data = wind_data_usa)

summary(mod_3_ols)

coeftest(mod_3_ols, vcov. = vcovHC, cluster = ~cen_tract_id)

# IVs: Demographics + Wind Plant Characteristics + Political + Region + Comm Own #

mod_4_ols <- lm(is_anti_wind ~ # DV = dummy variable indicating presence of anti-wind protest.
                  I(pop_den / 1000) + 
                  I(med_inc / 10000) + 
                  white_perc + 
                  his_lat_perc +
                  black_perc +
                  total_mw + 
                  n_turbs + 
                  region_meta +
                  gop_per,
                data = wind_data_usa)

summary(mod_4_ols)

coeftest(mod_4_ols, vcov. = vcovHC, cluster = ~cen_tract_id)



# Create Covariate List #

mod_covariates <- c("Population Density (mi^2, per 1,000)", "Median Income (per 10,000)",
                    "Percent White", "Percent Hispanic/Latino", "Percent Black", "Community Ownership (AWEA)", "Total Megawatts",  "Number of Turbines", "Turbine Height",
                    "Northeast", "South", "West",  "Percent GOP Votes")


# OLS Models Table #

stargazer(mod_1_ols, mod_2_ols, mod_3_ols, mod_4_ols,
          type = "latex",
          covariate.labels = mod_covariates,
          column.sep.width = "-15pt")

# OLS Models Table with Clustered Standard Errors#

mod_ols_list <- list(mod_1_ols, mod_2_ols, mod_3_ols, mod_4_ols)

se_cluster <- function(x)
  coeftest(x, vcov. = vcovHC, cluster = ~cen_tract_id)[, "Std. Error"]

stargazer(mod_ols_list,
          type = "latex",
          covariate.labels = mod_covariates,
          column.sep.width = "-15pt", 
          se = lapply(mod_ols_list, se_cluster))


# Cancelled v Non-cancelled  (SI) # 
wind_sub <- wind_data_usa[which(wind_data_usa$is_anti_wind == 1), ]

wind_sub$status_2 <- as.character(wind_sub$status)
wind_sub$status_2[which(wind_sub$status == "Cancelled")] <- 1
wind_sub$status_2[which(wind_sub$status != "Cancelled")] <- 0
wind_sub$status_2[which(is.na(wind_sub$status) )] <- 0
wind_sub$status_2 <- as.numeric(wind_sub$status_2)

cancelled_tab <- t(aggregate(wind_sub[ , c("pop_den", "med_inc", "white_perc", "his_lat_perc","black_perc" , "gop_per", "total_mw", "n_turbs")],
                             list(wind_sub$status_2),
                             function(x) 
                               mean(x, na.rm = T)))

round(cancelled_tab,2)

# T-tests On Relevant Variables #

# Demographics #

t.test(wind_sub$pop_den[wind_sub$status_2 == 1], 
       wind_sub$pop_den[wind_sub$status_2 == 0])

t.test(wind_sub$med_inc[wind_sub$status_2 == 1], 
       wind_sub$med_inc[wind_sub$status_2 == 0])

t.test(wind_sub$white_perc[wind_sub$status_2 == 1], 
       wind_sub$white_perc[wind_sub$status_2 == 0])

t.test(wind_sub$his_lat_perc[wind_sub$status_2 == 1], 
       wind_sub$his_lat_perc[wind_sub$status_2 == 0])

t.test(wind_sub$black_perc[wind_sub$status_2 == 1], 
       wind_sub$black_perc[wind_sub$status_2 == 0])

t.test(wind_sub$gop_per[wind_sub$status_2 == 1], 
       wind_sub$gop_per[wind_sub$status_2 == 0])

t.test(wind_sub$total_mw[wind_sub$status_2 == 1], 
       wind_sub$total_mw[wind_sub$status_2 == 0])

t.test(wind_sub$n_turbs[wind_sub$status_2 == 1], 
       wind_sub$n_turbs[wind_sub$status_2 == 0])

# Table of Canceled v. Operational Opposed Projs #
table(wind_sub$status_2)

