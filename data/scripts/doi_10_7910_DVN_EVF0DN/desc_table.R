### descriptive table of variables
library(tidyverse)
library(here)
library(modelsummary)
library(kableExtra)
config_modelsummary(factory_latex = "kableExtra")

### define output paths
tab.path <- here("tables")
fig.path <- here("figures")

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))

#### multiply outcomes by 100 for percentage point interpretation
rail.df$separatism_cw_sec_ind_aut_yn_100 <- 100*rail.df$separatism_cw_sec_ind_aut_yn
rail.df$secession_yn <- 100*rail.df$secession_yn
rail.df$onset_nc_ind_aut <- 100*rail.df$onset_nc_ind_aut
rail.df$cw_terr_onset_combined <- 100*rail.df$cw_terr_onset_combined



### define countries to exclude from analysis
drop_cntrs <- c("United Kingdom","Egypt","Cherkessy","Ireland")

## code country_year fe
rail.df$country_year <- factor(paste(rail.df$id_cap_rule_corr,rail.df$year,sep="_"))

rail.df$ma.nat.donal.log[is.infinite(rail.df$ma.nat.donal.log)] <- NA
rail.df$popc_log <- log(rail.df$popc_corr)

##### subset to analysis sample
rail.df.sample <- rail.df[which(rail.df$rail_ttt_5years_binned%in%c(seq(-60,90,5),-1000,1000) &
                                  rail.df$year>1815  & rail.df$year<1946 &
                                  rail.df$capital_yn_dist_corr==0 &
                                  rail.df$area_sqkm_raw>2000 &
                                  !rail.df$name_cap_rule_corr%in%drop_cntrs),]

rail.df.sample$rail_cohort_year[rail.df.sample$rail_cohort_year > 1922] <- NA

# Select variables
vec_variables <- c("separatism_cw_sec_ind_aut_yn_100",
                   "secession_yn",
                   "onset_nc_ind_aut",
                   "cw_terr_onset_combined",
                   "rail_any_yn",
                   "rail_cohort_year",
                   "ma.nat.donal.log",
                   "state_cap",
                   "mob_cap",
                   "distance",
                   "pop_share_cntr_alt_cap",
                   "popc_log",
                   "gdppc_log",
                   "v2stfisccap",
                   "v2x_liberal")

# Function that produces one row with descriptives per variable
get_descriptives <- function(data, variable) {
  
  dat <- tibble(
    name = variable,
    "   " = "",
    Min = min(data[, variable], na.rm = T),
    Mean = mean(data[, variable], na.rm = T),
    Median = median(data[, variable], na.rm = T),
    Max = max(data[, variable], na.rm = T),
    "Std. dev." = sd(data[, variable], na.rm = T)
  )
  
  return(dat)
}

dat_desc <- map(vec_variables, ~ get_descriptives(data = rail.df.sample, .x)) %>% 
  bind_rows()

# Recode variables
dat_desc <- dat_desc %>% 
  mutate(name = dplyr::recode(name,
                              "separatism_cw_sec_ind_aut_yn_100" = "Combined outcome*",
                              "secession_yn" = "Successful secession*",
                              "onset_nc_ind_aut" = "First claim*",
                              "cw_terr_onset_combined" = "Civil war*",
                              "rail_any_yn"= "Rails (Y/N)",
                              "rail_cohort_year" = "First railway year",
                              "ma.nat.donal.log"= "National Market Access",
                              "state_cap" = "State Reach",
                              "mob_cap" = "Internal Connectivity",
                              "distance" = "Ling. Dist to Core",
                              "pop_share_cntr_alt_cap"="Pop. Share Core Group",
                              "popc_log"="Group Population (log)",
                              "gdppc_log"="GDP per capita (log)",
                              "v2stfisccap"="Fiscal Capacity (VDEM)",
                              "v2x_liberal"="Liberal Democracy (VDEM)"))

# Make table
tab_desc <- dat_desc %>% 
  rename("  " = name) %>% 
  kbl("latex",
      caption = "Descriptive statistics",
      label = "tab:descriptives",
      linesep = "",
      booktabs = T,
      position = "bh",
      digits = 3,
      align = c("l", rep("c", 6))
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>% 
  kableExtra::add_footnote("* Note: The outcome is multiplied by 100 to improve legibility.",
                           notation = "none")
kableExtra::save_kable(tab_desc, file = here("tables", "table_a1.tex"))
